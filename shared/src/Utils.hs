{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Exception (IOException, catch)
import Data.Char (toUpper)
import System.FilePath ((</>))
import System.IO

data Match = Correct | Present | Absent deriving (Eq, Show)

-- ANSI color codes
greenText :: String -> String
greenText s = "\ESC[32m" ++ s ++ "\ESC[0m"

redText :: String -> String
redText s = "\ESC[31m" ++ s ++ "\ESC[0m"

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
