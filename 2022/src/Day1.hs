module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.List

day1part1 :: String -> String
day1part1 = show . maximum . map sum . readChunksOf intParser

day1part2 :: String -> String
day1part2 = show . sum . take 3 . reverse . sort . map sum . readChunksOf intParser
