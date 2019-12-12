module Day4
  (day4part1, day4part2)
  where

import           Data.List

input :: [Int]
input = [168630 .. 718098]

hasSameAdjacent :: Int -> Bool
hasSameAdjacent = any ((> 1) . length) . group . show

atLeastOnePairAdjacent :: Int -> Bool
atLeastOnePairAdjacent = any ((== 2) . length) . group . show

allIncreasing :: Int -> Bool
allIncreasing i = all (uncurry (<=)) $ zip (show i) (tail $ show i)

day4part1 :: String -> String
day4part1 _ = show $ length $ filter hasSameAdjacent $ filter allIncreasing input

day4part2 :: String -> String
day4part2 _ = show $ length $ filter atLeastOnePairAdjacent $ filter allIncreasing input
