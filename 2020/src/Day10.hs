module Day10
  (day10part1, day10part2)
  where

import           Common
import           Data.List

day10part1 :: String -> String
day10part1 = show . product . map length . group . sort . map (\(x,y) -> y - x) . pairs . filledList . sort . readListOf intParser
  where
    filledList xs = [0] ++ xs ++ [maximum xs + 3]
    pairs xs = zip xs (tail xs)

day10part2 :: String -> String
day10part2 _ = ""
