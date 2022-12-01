module Day1
  (day1part1, day1part2)
  where

import           Common

day1part1 :: String -> String
day1part1 = show . maximum . map sum . readChunksOf intParser

day1part2 :: String -> String
day1part2 _ = ""
