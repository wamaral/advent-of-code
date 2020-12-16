{-# LANGUAGE Strict #-}

module Day15
  (day15part1, day15part2)
  where

import           Common
import           Control.Monad.Loops
import           Control.Monad.ST
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as MV
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Game s = Game
  { index   :: Int
  , number  :: Int
  , visited :: MV.MVector s Int
  }

inputParser :: Parser [Int]
inputParser = intParser `sepBy` char ',' <* optional newline

prefillGame :: Int -> [Int] -> ST s (Game s)
prefillGame len xs = do
  vec <- MV.replicate len minBound
  let fillPairs = zip xs [1..(pred $ length xs)]
  mapM_ (\(num,idx) -> MV.write vec num idx) fillPairs
  return $ Game {index = length xs, number = lastDef minBound xs, visited = vec}

nextNumber :: Game s -> ST s (Game s)
nextNumber game = do
  let vec = visited game
  let newIndex = succ $ index game
  newNumber <- (\x -> max 0 (index game - x)) <$> MV.read vec (number game)
  MV.write vec (number game) (index game)
  return $ Game {index = newIndex, number = newNumber, visited = vec}

runFor :: Int -> String -> ST s String
runFor n input = do
  let parsedInput = fromMaybe [] $ parseMaybe inputParser input
  baseGame <- prefillGame n parsedInput
  finalGame <- iterateUntilM (\g -> index g >= n) nextNumber baseGame
  return $ show $ number finalGame

day15part1 :: String -> String
day15part1 input = runST $ runFor 2020 input

day15part2 :: String -> String
day15part2 input = runST $ runFor 30000000 input
