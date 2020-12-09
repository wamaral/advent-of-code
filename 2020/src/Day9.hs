module Day9
  (day9part1, day9part2)
  where

import           Common
import           Control.Monad
import           Data.Function
import           Data.List.Split
import           Safe

isSumOfAnyPair :: Int -> [Int] -> Bool
isSumOfAnyPair x xs = x `elem` allSums
  where allSums = map sum $ replicateM 2 xs

notSums :: [Int] -> [Int]
notSums xs = map fst $ filter (not . uncurry isSumOfAnyPair) allTests
  where allTests = divvy 26 1 xs & map ((\(y:ys) -> (y, ys)) . reverse)

day9part1 :: String -> String
day9part1 = show . headDef 0 . notSums . readListOf intParser

day9part2 :: String -> String
day9part2 _ = ""
