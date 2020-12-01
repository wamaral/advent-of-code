module Day1
  (day1part1, day1part2)
  where

import           Common
import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Safe

solveForLength :: Int -> [Int] -> Int
solveForLength n xs = replicateM n xs
  & filter (\xs' -> sum xs' == 2020)
  & map product
  & headMay
  & fromMaybe 0

day1part1 :: String -> String
day1part1 = show . solveForLength 2 . readListOf intParser

day1part2 :: String -> String
day1part2 = show . solveForLength 3 . readListOf intParser
