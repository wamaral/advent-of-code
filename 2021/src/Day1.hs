module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.List.Split

day1part1 :: String -> String
day1part1 = show . length . concat . splitWhen (uncurry (>)) . zipTail . readListOf intParser

day1part2 :: String -> String
day1part2 _ = ""
