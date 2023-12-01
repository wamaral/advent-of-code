module Day1
  (day1part1, day1part2)
  where

import Common
import Data.Char (isDigit)

firstLast :: [a] -> [a]
firstLast [] = []
firstLast xs = [head xs, last xs]

day1part1 :: String -> String
day1part1 = show . sum . map (stringToInt0 . firstLast . filter isDigit) . lines

day1part2 :: String -> String
day1part2 _ = ""
