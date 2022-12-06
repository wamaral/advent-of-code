module Day6
  (day6part1, day6part2)
  where

import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set      as S

day6part1 :: String -> String
day6part1 input = zipWith4 (\a b c d -> [a,b,c,d]) input (drop 1 input) (drop 2 input) (drop 3 input)
  & findIndex allDistinct
  & fmap (+ 4)
  & fromMaybe 0
  & show
  where allDistinct = (== 4) . S.size . S.fromList

day6part2 :: String -> String
day6part2 _ = ""
