{-# LANGUAGE TupleSections   #-}

module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.Bifunctor
import           Data.Function
import qualified Data.IntMap.Strict as M
import           Data.List
import           Text.Megaparsec.Char

type IntPair = (Int, Int)

intPairParser :: Parser IntPair
intPairParser = do
  l1 <- intParser
  _ <- hspace1
  l2 <- intParser
  return (l1, l2)

day1part1 :: String -> String
day1part1 input = readListOf intPairParser input
  & unzip
  & bimap sort sort
  & uncurry (zipWith (-))
  & map abs
  & sum
  & show

day1part2 :: String -> String
day1part2 input = list1
  & map (\n -> n * M.findWithDefault 0 n mapCount)
  & sum
  & show
  where
    (list1, list2) = unzip $ readListOf intPairParser input
    mapCount = M.fromListWith (+) (map (,1) list2)
