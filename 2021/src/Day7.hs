module Day7
  (day7part1, day7part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

crabParser :: Parser [Int]
crabParser = intParser `sepBy` char ',' <* optional newline

parseInput :: String -> [Int]
parseInput = fromMaybe [] . parseMaybe crabParser

-- From https://hackage.haskell.org/package/dsp-0.2.5.1/docs/src/Numeric.Statistics.Median.html#median
-- Converted to return Integral instead of Fractional
median :: (Ord a, Integral a) => [a] -> a
median x =
  if odd n
    then sort x !! (n `div` 2)
    else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) `div` 2
  where n = length x

mean :: (Integral a) => [a] -> Double
mean xs = realToFrac (sum xs) / genericLength xs

sumUpTo :: Int -> Int
sumUpTo n = n * succ n `div` 2

day7part1 :: String -> String
day7part1 input = crabs
  & map (\x -> abs $ x - pos)
  & sum
  & show
  where
    crabs = parseInput input
    pos = median crabs

day7part2 :: String -> String
day7part2 input = crabs
  & posCandidates
  & map (sum . solve crabs)
  & minimum
  & show
  where
    crabs = parseInput input
    posCandidates c = map ($ mean c) [floor, ceiling]
    solve c pos = map (\c' -> sumUpTo (abs $ c' - pos)) c
