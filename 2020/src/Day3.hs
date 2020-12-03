module Day3
  (day3part1, day3part2)
  where

import           Common
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Square = Open | Tree deriving (Eq, Show)

squareParser :: Parser Square
squareParser = choice
  [ Open <$ char '.'
  , Tree <$ char '#'
  ]

squareAtPos :: Int -> [Square] -> Maybe Square
squareAtPos n squares = atMay (cycle squares) n

day3part1 :: String -> String
day3part1 = show . length . filter (== Tree) . mapMaybe (uncurry squareAtPos) . zip [3,6..] . tail . readListOf (many squareParser)

day3part2 :: String -> String
day3part2 _ = ""
