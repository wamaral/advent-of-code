module Day6
  (day6part1, day6part2)
  where

import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Set        as S

allDistinct :: String -> Bool
allDistinct list = S.fromList list
  & S.size
  & (== length list)

findPosition :: Int -> String -> Int
findPosition countDistinct input = divvy countDistinct 1 input
  & findIndex allDistinct
  & fmap (+ countDistinct)
  & fromMaybe 0

day6part1 :: String -> String
day6part1 = show . findPosition 4

day6part2 :: String -> String
day6part2 = show . findPosition 14
