module Day23
  (day23part1, day23part2)
  where

import           Common
import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

inputParser :: Parser [Int]
inputParser = map (stringToInt0 . (: [])) <$> many digitChar <* optional newline <* eof

parseInput :: String -> [Int]
parseInput = fromMaybe [] . parseMaybe inputParser

-- | Creates a mutable vector where the index is the "cup" and the value at that
-- index is the cup it points to, so kinda faking a linked list
initCups :: Int -> [Int] -> ST s (MV.STVector s Int)
initCups totalCups base = do
  let allCups = base ++ [succ (length base) .. totalCups]
  let cupPairs = zip allCups (tail $ cycle allCups)
  vec <- MV.new (succ $ length cupPairs)
  mapM_ (uncurry (MV.write vec)) cupPairs
  return vec

runStep :: MV.STVector s Int -> Int -> ST s Int
runStep cups current = do
  a <- MV.read cups current -- Next 3 values
  b <- MV.read cups a
  c <- MV.read cups b
  next <- MV.read cups c -- Cup for next iteration
  let destination = destinationCandidates & (\\ [a,b,c]) & headDef 0
  destinationCup <- MV.read cups destination
  MV.write cups current next -- Set next cup
  MV.write cups destination a -- Pretend that we "shifted" the 3 values to destination
  MV.write cups c destinationCup -- Make 3rd cup point to destination cup
  return next
  where destinationCandidates = (iterate pred current & tail & take 4) ++ (iterate pred (MV.length cups) & tail & take 4) & filter (> 0)

runFor :: Int -> Int -> MV.STVector s Int -> ST s (MV.STVector s Int)
runFor moveCount firstCup cups = do
  foldM_ (\cup _ -> runStep cups cup) firstCup [1..moveCount]
  return cups

rotateTo :: Int -> [Int] -> [Int]
rotateTo n xs = break (== n) xs & (\(x, y) -> y ++ x)

cupsToList :: V.Vector Int -> [Int]
cupsToList v = foldl' (\acc _ -> acc ++ [(V.!) v (lastDef 1 acc)]) [] [1 .. (pred $ V.length v)]

solve1 :: MV.STVector s Int -> ST s String
solve1 v = do
  vec <- V.freeze v
  vec & cupsToList & rotateTo 1 & tailDef [] & concatMap show & return

solve2 :: MV.STVector s Int -> ST s Int
solve2 v = do
  a <- MV.read v 1
  b <- MV.read v a
  return (a * b)

day23part1 :: String -> String
day23part1 input = runST $ initCups cupCount parsedInput
  >>= runFor moveCount firstCup
  >>= solve1
  where
    parsedInput = parseInput input
    firstCup = headDef 0 parsedInput
    cupCount = length parsedInput
    moveCount = 100

day23part2 :: String -> String
day23part2 input = show $ runST $ initCups cupCount parsedInput
  >>= runFor moveCount firstCup
  >>= solve2
  where
    parsedInput = parseInput input
    firstCup = headDef 0 parsedInput
    cupCount = 1000000
    moveCount = 10000000
