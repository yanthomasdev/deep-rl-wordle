{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import DQN
import Data.List (drop, foldl', maximumBy, take)
import Data.Ord (comparing)
import Game
import System.Environment (getArgs)
import System.IO
import System.Random
import Utils

-- Network configuration
inputSize = 209

hiddenLayers = [512, 256, 128]

startLR = 0.001

endLR = 0.00005

discountFactor = 0.99

epsilonStart = 1.0

epsilonDecay = 0.999

epsilonMin = 0.02

epochs = 5000

batchSize = 64

bufferSize = 10000

type Transition = (Vector, Int, Float, Vector, Bool)

type ReplayBuffer = [Transition]

-- Gets the learning rate based on the current epoch, learning decaying
getLearningRate :: Int -> Float
getLearningRate currentEpoch =
  let progress = fromIntegral (epochs - currentEpoch) / fromIntegral epochs
   in endLR + (startLR - endLR) * progress

-- Masks the action to force it to make consistent guesses based on the history
-- This is effectively a way to force the agent to play Wordle's Hard Mode
-- consequentially, it also reduces the action space as the game progresses
isConsistent :: String -> [(String, [Match])] -> Bool
isConsistent candidate = all checkTurn
  where
    checkTurn (guess, matches) =
      all (\(c, g, m) -> m /= Correct || c == g) (zip3 candidate guess matches)
        && all (\(g, m) -> m /= Present || g `elem` candidate) (zip guess matches)

-- Letter discovery
getKnowns :: [(String, [Match])] -> ([Int], [Char])
getKnowns history = (greens, letters)
  where
    greens = [i | (_, matches) <- history, (i, Correct) <- zip [0 ..] matches]
    letters = concat [[c | (c, m) <- zip g ms, m == Correct || m == Present] | (g, ms) <- history]

calculateLetterScore :: String -> [Match] -> [(String, [Match])] -> Float
calculateLetterScore guess matches history = sum $ zipWith3 score [0 ..] guess matches
  where
    (knownPos, knownLetters) = getKnowns history
    score i c m = case m of
      Correct -> if i `elem` knownPos then 0.0 else 3.0
      Present -> if c `elem` knownLetters then 0.1 else 1.5
      Absent -> -0.1

-- Returns a reward based on how much the guess narrowed down the vocabulary
calculatePruningReward :: [String] -> [(String, [Match])] -> [(String, [Match])] -> Float
calculatePruningReward vocab oldHistory newHistory =
  let -- Count how many words were possible BEFORE this guess
      validBefore = length [w | w <- vocab, isConsistent w oldHistory]
      -- Count how many words are possible AFTER this guess
      validAfter = length [w | w <- vocab, isConsistent w newHistory]

      -- Prevent division by zero
      denom = if validBefore == 0 then 1.0 else fromIntegral validBefore
      num = fromIntegral (validBefore - validAfter)

      -- Percentage of the vocabulary eliminated (0.0 to 1.0)
      reductionRatio = num / denom
   in -- If 50% of words were eliminated -> Reward 5.0
      -- If 90% of words were eliminated -> Reward 9.0
      reductionRatio * 10.0

-- Action selection
selectAction :: Network -> Vector -> [Int] -> Int -> Float -> StdGen -> [(String, [Match])] -> [String] -> (Int, StdGen)
selectAction net state forbiddenIndices vocabSize eps gen history vocab =
  let (rnd, newGen) = uniformR (0.0, 1.0) gen

      validIndices =
        if null history
          then [0 .. vocabSize - 1]
          else [i | i <- [0 .. vocabSize - 1], isConsistent (vocab !! i) history]

      finalCandidates = if null validIndices then [0 .. vocabSize - 1] else validIndices
      playableIndices = filter (`notElem` forbiddenIndices) finalCandidates
      safeIndices = if null playableIndices then finalCandidates else playableIndices

      qValues = predict net state
      validQValues = filter (\(i, _) -> i `elem` safeIndices) (zip [0 ..] qValues)
   in if rnd < eps
        then
          let (ri, finalGen) = uniformR (0, length safeIndices - 1) newGen
           in (safeIndices !! ri, finalGen)
        else
          if null validQValues
            then (0, newGen)
            else
              let (bestIdx, _) = maximumBy (comparing snd) validQValues
               in (bestIdx, newGen)

getMatches (Won ms) = ms
getMatches (Lost ms) = ms
getMatches (InProgress ms) = ms

-- Episode runner
runEpisode :: Network -> StdGen -> Float -> [String] -> [String] -> IO ([Transition], Float, StdGen)
runEpisode net gen eps vocab solutions = do
  let (startState, g1) = initGame solutions gen

  let loop currState currGen totalReward history transitions forbiddenIndices = do
        let attemptsLeft = 6 - length history
        let stateVec = encodeGameHistory history attemptsLeft
        let vocabSize = length vocab

        let (actionIdx, g2) = selectAction net stateVec forbiddenIndices vocabSize eps currGen history vocab
        let guess = vocab !! actionIdx

        let (result, nextState) = play guess currState
        let matches = getMatches result
        let newHistory = (guess, matches) : history

        -- Reward calculation
        -- 1. Letter score (green/yellow letters found)
        let letterReward = calculateLetterScore guess matches history

        -- 2. Pruning score (possible words eliminated)
        -- This is meant to help avoid the edge case of words like _ATCH
        let pruningReward = calculatePruningReward vocab history newHistory

        let moveScore = letterReward + pruningReward

        let (reward, done) = case result of
              Won _ -> (100.0, True)
              Lost _ -> (-10.0, True)
              InProgress _ -> (moveScore, False)

        let nextAttempts = if done then 0 else attemptsLeft - 1
        let nextStateVec = encodeGameHistory newHistory nextAttempts

        let trans = (stateVec, actionIdx, reward, nextStateVec, done)
        let !() = forceTransition trans

        let newForbidden = actionIdx : forbiddenIndices
        let newTransitions = trans : transitions

        if done
          then return (reverse newTransitions, totalReward + reward, g2)
          else loop nextState g2 (totalReward + reward) newHistory newTransitions newForbidden

  (trans, r, gFinal) <- loop startState g1 0.0 [] [] []
  return (trans, r, gFinal)

replaceAtIndex idx val list = take idx list ++ [val] ++ drop (idx + 1) list

sampleBatch :: StdGen -> ReplayBuffer -> Int -> ([Transition], StdGen)
sampleBatch gen buffer size
  | length buffer <= size = (buffer, gen)
  | otherwise = let (indices, newGen) = randomIndices size (length buffer) gen in (map (buffer !!) indices, newGen)

randomIndices n maxVal g = foldl' (\(acc, gen) _ -> let (i, g') = uniformR (0, maxVal - 1) gen in (i : acc, g')) ([], g) [1 .. n]

-- Memory management helpers
forceTransition :: Transition -> ()
forceTransition (s, _, r, ns, _) = forceVector s `seq` forceVector ns `seq` r `seq` ()

-- Training loop
train :: Int -> Network -> StdGen -> Float -> [Float] -> ReplayBuffer -> [String] -> [String] -> Float -> IO ()
train 0 net _ _ rewards _ _ _ bestReward = do
  putStrLn "Training complete."

  let finalAvg = if null rewards then 0.0 else sum (take 50 rewards) / fromIntegral (min 50 (length rewards))
  putStrLn $ "Final avg reward: " ++ show finalAvg
  putStrLn $ "Best avg reward: " ++ show bestReward

  writeFile "wordle_dqn_last.txt" (show net)
  putStrLn "Final model saved."
train n net gen eps rewards buffer vocab solutions bestReward = do
  (newTransitions, reward, newGen) <- runEpisode net gen eps vocab solutions

  let tempBuffer = buffer ++ newTransitions
  let finalBuffer = if length tempBuffer > bufferSize then drop (length tempBuffer - bufferSize) tempBuffer else tempBuffer

  (trainedNet, gTrain) <-
    if length finalBuffer > batchSize
      then do
        let (batch, gBatch) = sampleBatch newGen finalBuffer batchSize
        let prepareSample (s, a, r, ns, d) =
              let nextQ = predict net ns
                  maxNext = if d then 0.0 else maximum nextQ
                  targetVal = r + discountFactor * maxNext
                  currentQ = predict net s
                  targetVec = replaceAtIndex a targetVal currentQ
               in (s, targetVec)

        let trainingData = map prepareSample batch
        let currentLR = getLearningRate n
        let updatedNet = trainBatch net trainingData currentLR

        return (updatedNet, gBatch)
      else return (net, newGen)

  -- Avoid memory leak by forcing evaluation
  let !() = forceNetwork trainedNet
  let newEps = max epsilonMin (eps * epsilonDecay)

  let currentAvg = if null rewards then -100.0 else sum (take 50 rewards) / fromIntegral (min 50 (length rewards))
  let (newBestReward, savedMsg) =
        if currentAvg > bestReward && n < (epochs - 200)
          then (currentAvg, " [NEW BEST! Saved]")
          else (bestReward, "")

  when (savedMsg /= "") $ writeFile "wordle_dqn_best.txt" (show trainedNet)

  when (n `mod` 50 == 0) $ do
    putStrLn $ "Epoch " ++ show (epochs - n) ++ " | Reward: " ++ show reward ++ " | Eps: " ++ take 5 (show eps) ++ " | Buffer: " ++ show (length finalBuffer) ++ savedMsg

  train (n - 1) trainedNet gTrain newEps (reward : rewards) finalBuffer vocab solutions newBestReward

demonstrate :: Network -> StdGen -> [String] -> IO ()
demonstrate net gen vocab = do
  let (startState, g1) = initGame vocab gen
  let target = solution startState

  putStrLn $ "\n----------------------------------------"
  putStrLn $ "Target Word: " ++ target
  putStrLn "----------------------------------------"

  let loop currState currGen history step forbiddenIndices = do
        let attemptsLeft = 6 - length history
        let vocabSize = length vocab

        let stateVec = encodeGameHistory history attemptsLeft

        let (actionIdx, g2) = selectAction net stateVec forbiddenIndices vocabSize 0.0 currGen history vocab

        let guess = vocab !! actionIdx
        let (result, nextState) = play guess currState
        let matches = getMatches result

        let coloredGuess = formatGuess guess matches
        putStrLn $ "Guess " ++ show step ++ ": " ++ coloredGuess

        let newHistory = (guess, matches) : history
        let newForbidden = actionIdx : forbiddenIndices

        case result of
          Won _ -> putStrLn $ greenText "*** Congratulations! You won! ***"
          Lost _ -> putStrLn $ redText "Game Over! You've run out of attempts."
          InProgress _ -> loop nextState g2 newHistory (step + 1) newForbidden

  loop startState g1 [] 1 []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  let demoMode = "--demonstrate" `elem` args
  let loadLast = "--load-last" `elem` args

  solutions <- loadWords "solutions.txt"

  if null solutions
    then putStrLn $ redText "Error: Could not load 'solutions.txt'."
    else do
      -- Vocabulary is being reduced to only solutions for faster training
      let vocab = solutions
      let actionSize = length vocab

      if demoMode
        then do
          putStrLn "=== DEMONSTRATION MODE ==="

          let modelFile = if loadLast then "wordle_dqn_last.txt" else "wordle_dqn_best.txt"
          putStrLn $ "Reading '" ++ modelFile ++ "'..."
          modelContent <- catch (readFile modelFile) (\(_ :: IOException) -> return "")

          if null modelContent
            then putStrLn $ redText "Error: Model not found."
            else do
              let net = read modelContent :: Network
              putStrLn "Model loaded. Playing 5 games..."

              gen <- getStdGen
              let seeds = take 5 $ iterate (snd . split) gen

              mapM_ (\g -> demonstrate net g vocab) seeds
        else do
          putStrLn "=== TRAINING MODE ==="
          putStrLn $ "Vocabulary: " ++ show actionSize
          putStrLn $ "Input: 209 | Hidden: " ++ show hiddenLayers

          gen <- getStdGen
          let (net, g1) = initNetwork ([inputSize] ++ hiddenLayers ++ [actionSize]) gen

          putStrLn "Starting training..."
          train epochs net g1 epsilonStart [] [] vocab solutions (-9999.0)
