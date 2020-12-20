module Day9
  (day9part1, day9part2, day9part1test, day9part2test)
  where

import           Common
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Split
import           Safe

isSumOfAnyPair :: Int -> [Int] -> Bool
isSumOfAnyPair x xs = x `elem` allSums
  where allSums = map sum $ replicateM 2 xs

notSums :: Int -> [Int] -> [Int]
notSums preamble xs = map fst $ filter (not . uncurry isSumOfAnyPair) allTests
  where allTests = divvy (succ preamble) 1 xs & map ((\(y:ys) -> (y, ys)) . reverse)

contiguousSumUntil :: Int -> [Int] -> [Int]
contiguousSumUntil n xs = scanl' (+) 0 xs
  & tail
  & takeWhile (<= n)
  & length
  & (`take` xs)

contiguousSumEqualTo :: Int -> [Int] -> [Int]
contiguousSumEqualTo n xs = map (contiguousSumUntil n) allTests
  & filter (\ys -> length ys >= 2)
  & filter (\ys -> n == sum ys)
  & headDef []
  where allTests = tails xs

solvePart1 :: Int -> String -> String
solvePart1 preamble = show . headDef 0 . notSums preamble . readListOf intParser

solvePart2 :: Int -> String -> String
solvePart2 preamble input = contiguousSumEqualTo invalidNum inputList
  & (\xs -> minimumDef 0 xs + maximumDef 0 xs)
  & show
  where
    inputList = readListOf intParser input
    invalidNum = headDef 0 $ notSums preamble inputList

day9part1 :: String -> String
day9part1 = solvePart1 25

day9part2 :: String -> String
day9part2 = solvePart2 25

-- Provided tests have a different "preamble" length than the official input :(

day9part1test :: String -> String
day9part1test = solvePart1 5

day9part2test :: String -> String
day9part2test = solvePart2 5
