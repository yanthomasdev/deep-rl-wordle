module DQN where

import Data.List (foldl', maximumBy, transpose)
import Data.Ord (comparing)
import System.Random (StdGen, uniformR)
import Utils

-- Types
type Vector = [Float]

type Matrix = [Vector]

type Weights = Matrix

type Biases = Vector

data Layer = Layer
  { weights :: Weights,
    biases :: Biases
  }
  deriving (Show, Read)

newtype Network = Network [Layer] deriving (Show, Read)

-- Encodes the game history into a fixed-size vector
encodeGameHistory :: [(String, [Match])] -> Int -> Vector
encodeGameHistory history attemptsLeft = boardVec ++ greenVec ++ yellowVec ++ grayVec ++ contextVec
  where
    -- Checks every letter 'A'..'Z' against the history
    charStatuses = map getCharStatus ['A' .. 'Z']

    getCharStatus c =
      let allMatchesForChar = [m | (word, matches) <- history, (l, m) <- zip word matches, l == c]
       in if null allMatchesForChar then Nothing else Just (bestMatch allMatchesForChar)

    bestMatch ms
      | Correct `elem` ms = Correct
      | Present `elem` ms = Present
      | otherwise = Absent

    -- One-hot encoding for letters
    greenVec = [if s == Just Correct then 1.0 else 0.0 | s <- charStatuses]
    yellowVec = [if s == Just Present then 1.0 else 0.0 | s <- charStatuses]
    grayVec = [if s == Just Absent then 1.0 else 0.0 | s <- charStatuses]

    -- Board Encoding (5 positions * 26 letters = 130 inputs)
    -- Uses foldl' to avoid thunk build-up
    knownBoard = foldl' mergeBoard (replicate 5 Nothing) history
    mergeBoard current (guess, matches) = zipWith3 update current guess matches
      where
        update (Just c) _ _ = Just c; update Nothing g Correct = Just g; update Nothing _ _ = Nothing

    boardVec = concatMap encodePos knownBoard
    encodePos Nothing = 1.0 : replicate 26 0.0 -- First bit is "Empty"
    encodePos (Just c) = 0.0 : [if x == c then 1.0 else 0.0 | x <- ['A' .. 'Z']]

    contextVec = [fromIntegral attemptsLeft / 6.0]

-- Math helpers
dot :: Vector -> Vector -> Float
dot v1 v2 = sum $ zipWith (*) v1 v2

addV :: Vector -> Vector -> Vector
addV = zipWith (+)

subV :: Vector -> Vector -> Vector
subV = zipWith (-)

scaleV :: Float -> Vector -> Vector
scaleV s = map (* s)

matVecMul :: Matrix -> Vector -> Vector
matVecMul m v = map (`dot` v) m

outer :: Vector -> Vector -> Matrix
outer v1 v2 = [[x * y | y <- v2] | x <- v1]

-- Leaky ReLU (helps avoid dying neurons)
relu :: Float -> Float
relu x = if x > 0 then x else 0.01 * x

reluDerivative :: Float -> Float
reluDerivative x = if x > 0 then 1.0 else 0.01

-- Initialization
randomList :: Int -> StdGen -> ([Float], StdGen)
randomList n g =
  let (vals, newG) = foldl' (\(acc, gen) _ -> let (x, g') = uniformR (-0.05, 0.05) gen in (x : acc, g')) ([], g) [1 .. n]
   in (vals, newG)

initLayer :: Int -> Int -> StdGen -> (Layer, StdGen)
initLayer inputSize outputSize g =
  let (b, g1) = randomList outputSize g
      (wFlat, g2) = randomList (inputSize * outputSize) g1
      w = chunk inputSize wFlat
   in (Layer w b, g2)
  where
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

initNetwork :: [Int] -> StdGen -> (Network, StdGen)
initNetwork sizes g =
  let (layers, finalG) = foldl' buildLayer ([], g) (zip sizes (tail sizes))
   in (Network (reverse layers), finalG)
  where
    buildLayer (acc, gen) (i, o) = let (l, newGen) = initLayer i o gen in (l : acc, newGen)

-- Forward Pass
forwardPass :: Network -> Vector -> ([Vector], [Vector])
forwardPass (Network layers) input =
  foldl' step ([input], []) layers
  where
    step (activations@(prevA : _), zs) layer =
      let z = addV (matVecMul (weights layer) prevA) (biases layer)
          isOutputLayer = length activations == length layers
          a = if isOutputLayer then z else map relu z
       in (a : activations, z : zs)

predict :: Network -> Vector -> Vector
predict net input = head $ fst $ forwardPass net input

-- Backpropagation
backprop :: Network -> Vector -> Vector -> [Layer]
backprop net@(Network layers) input target =
  let (activations, zs) = forwardPass net input
      finalOutput = head activations
      outputError = subV finalOutput target
      (grads, _) = foldl' calcGrad ([], outputError) (zip3 (reverse layers) (tail activations) zs)
   in grads
  where
    calcGrad (accGrads, delta) (layer, prevA, z) =
      let wGrad = outer delta prevA
          bGrad = delta
          wT = transpose (weights layer)
          prevError = matVecMul wT delta
          prevDelta = zipWith (*) prevError (map reluDerivative z)
          newLayerGrad = Layer wGrad bGrad
       in (newLayerGrad : accGrads, prevDelta)

clip :: Float -> Float
clip x | isNaN x = 0.0 | x > 1.0 = 1.0 | x < -1.0 = -1.0 | otherwise = x

addGrads :: [Layer] -> [Layer] -> [Layer]
addGrads = zipWith (\(Layer w1 b1) (Layer w2 b2) -> Layer (zipWith addV w1 w2) (addV b1 b2))

scaleGrads :: Float -> [Layer] -> [Layer]
scaleGrads factor = map (\(Layer w b) -> Layer (map (scaleV factor) w) (scaleV factor b))

-- Forces better memory management by evaluating all elements
forceVector :: Vector -> ()
forceVector v = sum v `seq` ()

forceMatrix :: Matrix -> ()
forceMatrix m = sum (map sum m) `seq` ()

forceLayer :: Layer -> ()
forceLayer (Layer w b) = forceMatrix w `seq` forceVector b

forceNetwork :: Network -> ()
forceNetwork (Network layers) = foldl' (\_ l -> forceLayer l) () layers

-- Batch training
trainBatch :: Network -> [(Vector, Vector)] -> Float -> Network
trainBatch net batch learningRate =
  let batchSize = fromIntegral (length batch)
      allGrads = map (\(inp, tgt) -> backprop net inp tgt) batch
      summedGrads = foldl1 addGrads allGrads
      avgGrads = scaleGrads (1.0 / batchSize) summedGrads

      newNet = updateNetwork net avgGrads learningRate
   in forceNetwork newNet `seq` newNet

-- Update weights
updateNetwork :: Network -> [Layer] -> Float -> Network
updateNetwork (Network layers) grads learningRate =
  Network $ zipWith updateLayer layers grads
  where
    updateLayer (Layer w b) (Layer dw db) =
      let applyUpdate param grad = param - learningRate * (clip grad)
          newW = zipWith (zipWith applyUpdate) w dw
          newB = zipWith applyUpdate b db
       in Layer newW newB
