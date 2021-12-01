module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.List.Split

countIncreases :: [Int] -> Int
countIncreases = length . filter (uncurry (<)) . zipTail

day1part1 :: String -> String
day1part1 = show . countIncreases . readListOf intParser

day1part2 :: String -> String
day1part2 = show . countIncreases . map sum . divvy 3 1 . readListOf intParser
