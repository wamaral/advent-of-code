module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Safe

allPairs :: [Int] -> [(Int, Int)]
allPairs xs = nub [(x,y) | x <- xs, y <- xs, x /= y]

day1part1 :: String -> String
day1part1 input = readListOf intParser input
  & allPairs
  & filter (\(x,y) -> x + y == 2020)
  & map (\(x,y) -> x * y)
  & headMay
  & fromMaybe 0
  & show

day1part2 :: String -> String
day1part2 _ = ""
