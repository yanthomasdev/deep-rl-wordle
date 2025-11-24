module Game where

import Data.List (elemIndices, mapAccumL)
import System.Random (RandomGen, uniformR)
import Utils (Match (..))

data GameResult = Won [Match] | Lost [Match] | InProgress [Match] deriving (Eq, Show)

data GameState = GameState
  { solution :: String,
    attemptsLeft :: Int
  }
  deriving (Show)

checkGuess :: String -> String -> [Match]
checkGuess guess solution = checkResult
  where
    -- Identify all exact matches (correct letter and position)
    exactMatches :: [Bool]
    exactMatches = zipWith (==) guess solution

    -- Count frequency of letter in a string
    countFreq :: String -> [(Char, Int)]
    countFreq str = [(c, length $ filter (== c) str) | c <- ['A' .. 'Z']]

    -- Count letters for present matches (correct letter but wrong position)
    presentFreq :: [(Char, Int)]
    presentFreq = countFreq [t | (t, isGreen) <- zip solution exactMatches, not isGreen]

    -- Decrement frequency count for a letter
    decrement :: Char -> [(Char, Int)] -> [(Char, Int)]
    decrement c [] = []
    decrement c ((x, n) : xs)
      | c == x = (x, n - 1) : xs
      | otherwise = (x, n) : decrement c xs

    -- Determine the guess result
    (_, checkResult) = mapAccumL determineResult presentFreq (zip guess exactMatches)

    determineResult :: [(Char, Int)] -> (Char, Bool) -> ([(Char, Int)], Match)
    determineResult available (g, isGreen)
      | isGreen = (available, Correct)
      | otherwise =
          case lookup g available of
            Just n | n > 0 -> (decrement g available, Present)
            _ -> (available, Absent)

-- Initialize a new game with a random solution from the list
initGame :: (RandomGen g) => [String] -> g -> (GameState, g)
initGame solutions gen =
  let (idx, newGen) = uniformR (0, length solutions - 1) gen
      selectedSolution = solutions !! idx
   in (GameState selectedSolution 6, newGen)

-- Play a round of the game with the given guess
play :: String -> GameState -> (GameResult, GameState)
play guess gameState
  | attemptsLeft gameState <= 0 = (Lost [], gameState)
  | otherwise =
      let matches = checkGuess guess (solution gameState)
          newState = gameState {attemptsLeft = attemptsLeft gameState - 1}
       in if all (== Correct) matches
            then (Won matches, newState)
            else
              if attemptsLeft newState == 0
                then (Lost matches, newState)
                else (InProgress matches, newState)
