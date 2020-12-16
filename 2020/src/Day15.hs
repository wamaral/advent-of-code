{-# LANGUAGE Strict #-}

module Day15
  (day15part1, day15part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Game = Game
  { index   :: Int
  , number  :: Int
  , visited :: M.Map Int Int
  } deriving Show

inputParser :: Parser [Int]
inputParser = intParser `sepBy` char ',' <* optional newline

prefillGame :: [Int] -> Game
prefillGame xs = Game
  { index = length xs
  , number = lastDef minBound xs
  , visited = foldl' (\m (i, x) -> M.insert x i m) M.empty $ zip [1..(pred $ length xs)] xs
  }

nextNumber :: Game -> Game
nextNumber game = Game
  { index = newIndex
  , number = newNumber
  , visited = M.insert (number game) (index game) (visited game)
  }
  where
    newIndex = succ $ index game
    newNumber = M.lookup (number game) (visited game) & fmap (\x -> index game - x) & fromMaybe 0

runFor :: Int -> String -> String
runFor n input = iterate nextNumber baseGame
  & take (n - pred (length parsedInput))
  & last
  & number
  & show
  where
    parsedInput = fromMaybe [] $ parseMaybe inputParser input
    baseGame = prefillGame parsedInput

day15part1 :: String -> String
day15part1 = runFor 2020

day15part2 :: String -> String
day15part2 = runFor 30000000
