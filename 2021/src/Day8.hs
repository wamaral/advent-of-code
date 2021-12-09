{-# LANGUAGE RecordWildCards #-}

module Day8
  (day8part1, day8part2)
  where

import           Common
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Display = Display { signals :: [String], output :: [String] } deriving Show

wireParser :: Parser Char
wireParser = oneOf "abcdefg"

displayParser :: Parser Display
displayParser = do
  signals <- someTill (some wireParser <* hspace) $ string "| "
  output <- some wireParser `sepBy` hspace
  return Display{..}

day8part1 :: String -> String
day8part1 = show . length . filter is1478 . concatMap output . readListOf displayParser
  where is1478 n = length n `elem` [2, 3, 4, 7]

day8part2 :: String -> String
day8part2 _ = ""
