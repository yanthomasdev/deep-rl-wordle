{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (IOException, catch)
import Data.Char (toUpper)
import Data.List (intercalate)
import Game
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering, NoBuffering), hFlush, hSetBuffering, hSetEncoding, stdin, stdout, utf8)
import System.Random (getStdGen)

-- ANSI color codes
greenBg :: String -> String
greenBg s = "\ESC[42m\ESC[30m " ++ s ++ " \ESC[0m"

yellowBg :: String -> String
yellowBg s = "\ESC[43m\ESC[30m " ++ s ++ " \ESC[0m"

grayBg :: String -> String
grayBg s = "\ESC[100m\ESC[37m " ++ s ++ " \ESC[0m"

-- Format a single letter with its match result
formatLetter :: Char -> Match -> String
formatLetter c Correct = greenBg [c]
formatLetter c Present = yellowBg [c]
formatLetter c Absent = grayBg [c]

-- Format an entire guess with its matches
formatGuess :: String -> [Match] -> String
formatGuess guess matches =
  unwords $ zipWith formatLetter guess matches

-- Display the game board with all previous guesses
displayBoard :: [(String, [Match])] -> Int -> IO ()
displayBoard guesses attemptsLeft = do
  putStrLn "\n=== WORDLE ==="
  putStrLn ""
  mapM_ (\(g, m) -> putStrLn $ "  " ++ formatGuess g m) guesses
  putStrLn ""
  putStrLn $ "Attempts remaining: " ++ show attemptsLeft
  putStrLn ""

-- Load words from a file
loadWordsFromFile :: FilePath -> IO [String]
loadWordsFromFile filepath = do
  contents <- readFile filepath
  return $ map (map toUpper) $ filter (\w -> length w == 5) $ lines contents

-- Try multiple possible file paths
loadWords :: FilePath -> IO [String]
loadWords filename = do
  let paths =
        [ filename, -- Current directory
          ".." </> ".." </> filename, -- From wordle/app/
          ".." </> filename -- From wordle/
        ]
  tryPaths paths
  where
    tryPaths [] = return []
    tryPaths (p : ps) = do
      result <- (loadWordsFromFile p >> return p) `catch` \(_ :: IOException) -> return ""
      if null result
        then tryPaths ps
        else do
          putStrLn $ "[OK] Loaded words from: " ++ p
          loadWordsFromFile p

-- Main game loop
gameLoop :: [String] -> [String] -> GameState -> [(String, [Match])] -> IO ()
gameLoop guesses solutions gameState history = do
  displayBoard history (attemptsLeft gameState)

  putStr "Enter your guess (5 letters): "
  hFlush stdout
  input <- getLine
  let guess = map toUpper input

  -- Validate input
  if length guess /= 5
    then do
      putStrLn "[!] Please enter exactly 5 letters!"
      gameLoop guesses solutions gameState history
    else
      if guess `notElem` guesses && guess `notElem` solutions
        then do
          putStrLn "[!] Invalid guess! Please try a valid 5-letter word."
          gameLoop guesses solutions gameState history
        else do
          let (result, newState) = play guess gameState

          case result of
            Won matches -> do
              let newHistory = history ++ [(guess, matches)]
              displayBoard newHistory (attemptsLeft newState)
              putStrLn "*** Congratulations! You won! ***"
              putStrLn $ "The word was: " ++ solution gameState
            Lost matches -> do
              let newHistory = history ++ [(guess, matches)]
              displayBoard newHistory (attemptsLeft newState)
              putStrLn "Game Over! You've run out of attempts."
              putStrLn $ "The word was: " ++ solution gameState
            InProgress matches -> do
              let newHistory = history ++ [(guess, matches)]
              gameLoop guesses solutions newState newHistory

main :: IO ()
main = do
  -- Set up terminal
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering

  -- Load words from file with error handling
  putStrLn "Loading word lists..."
  solutions <- loadWords "solutions.txt"
  guesses <- loadWords "guesses.txt"

  -- Check if we have valid words
  if null solutions
    then putStrLn "[X] No valid 5-letter words found!"
    else do
      -- Initialize game
      gen <- getStdGen
      let (gameState, _) = initGame solutions gen

      -- Welcome message
      putStrLn "\n================================"
      putStrLn "       Welcome to WORDLE!       "
      putStrLn "================================"
      putStrLn ""
      putStrLn "Guess the 5-letter word in 6 tries."
      putStrLn ""
      putStrLn "Color coding:"
      putStrLn $ "  " ++ greenBg "A" ++ " = Correct letter in correct position"
      putStrLn $ "  " ++ yellowBg "B" ++ " = Correct letter in wrong position"
      putStrLn $ "  " ++ grayBg "C" ++ " = Letter not in word"
      putStrLn ""
      putStrLn "Press Ctrl+C to quit at any time."
      putStrLn ""
      putStrLn $ "Playing with " ++ show (length solutions) ++ " possible solutions."
      putStrLn ""

      -- Start game loop
      gameLoop guesses solutions gameState []
