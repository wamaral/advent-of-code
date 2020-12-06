module Day6
  (day6part1, day6part2)
  where

import           Data.List
import           Data.List.Split

day6part1 :: String -> String
day6part1 = show . sum . map (length . foldr1 union) . splitOn [""] . lines

day6part2 :: String -> String
day6part2 = show . sum . map (length . foldr1 intersect) . splitOn [""] . lines
