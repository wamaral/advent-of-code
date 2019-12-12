module Day4
  (day4part1, day4part2)
  where

import           Data.List

lowInput :: Int
lowInput = 168630

highInput :: Int
highInput = 718098

hasSameAdjacent :: Int -> Bool
hasSameAdjacent = any ((> 1) . length) . group . show

allIncreasing :: Int -> Bool
allIncreasing i = all (uncurry (<=)) $ zip (show i) (tail $ show i)

day4part1 :: String -> String
day4part1 _ = show $ length $ filter hasSameAdjacent $ filter allIncreasing [lowInput .. highInput]

day4part2 :: String -> String
day4part2 _ = ""
