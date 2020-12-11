module Day10
  (day10part1, day10part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.List.Split

paddedInput :: [Int] -> [Int]
paddedInput xs = [0] ++ xs ++ [maximum xs + 3]

allConnectable :: [Int] -> Bool
allConnectable xs = zipTail xs & all isSeq
  where isSeq (x,y) = y - x <= 3

allCombinations :: [Int] -> [[Int]]
allCombinations xs = subsequences xs & filter (\x -> head xs `elem` x && last xs `elem` x) & filter allConnectable

day10part1 :: String -> String
day10part1 = show . product . map length . group . sort . map (\(x,y) -> y - x) . zipTail . paddedInput . sort . readListOf intParser

day10part2 :: String -> String
day10part2 = show . product . map (length . allCombinations . joinPairList) . filter (not . null) . splitWhen (not . sequential) . zipTail . paddedInput . sort . readListOf intParser
  where
    sequential (x,y) = y - x == 1
    joinPairList = nub . concatMap (\(x,y) -> [x,y])
