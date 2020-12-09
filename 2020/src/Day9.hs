module Day9
  (day9part1, day9part2)
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

notSums :: [Int] -> [Int]
notSums xs = map fst $ filter (not . uncurry isSumOfAnyPair) allTests
  where allTests = divvy 26 1 xs & map ((\(y:ys) -> (y, ys)) . reverse)

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

day9part1 :: String -> String
day9part1 = show . headDef 0 . notSums . readListOf intParser

day9part2 :: String -> String
day9part2 input = contiguousSumEqualTo invalidNum inputList
  & (\xs -> minimum xs + maximum xs)
  & show
  where
    inputList = readListOf intParser input
    invalidNum = headDef 0 $ notSums inputList
