module Day15
  (day15part1, day15part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

inputParser :: Parser [Int]
inputParser = intParser `sepBy` char ',' <* optional newline

nextNumber :: [Int] -> [Int]
nextNumber xs = xs ++ [prevN]
  where
    n = lastDef 0 xs
    prevN = reverse xs & tailDef [] & elemIndex n & fmap succ & fromMaybe 0

day15part1 :: String -> String
day15part1 input = iterate nextNumber parsedInput
  & take (2020 - pred (length parsedInput))
  & last
  & last
  & show
  where parsedInput = fromMaybe [] $ parseMaybe inputParser input

day15part2 :: String -> String
day15part2 _ = ""
