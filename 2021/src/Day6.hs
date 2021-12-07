module Day6
  (day6part1, day6part2)
  where

import           Common
import           Data.Function
import qualified Data.IntMap          as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Aquarium = M.IntMap Integer

fishParser :: Parser [Int]
fishParser = intParser `sepBy` char ',' <* optional newline

parseInput :: String -> [Int]
parseInput = fromMaybe [] . parseMaybe fishParser

startingFish :: [Int] -> Aquarium
startingFish = M.fromListWith (+) . flip zip (repeat 1)

simulate :: Aquarium -> Aquarium
simulate old = new
  & M.insertWith (+) 6 procreating
  & M.insertWith (+) 8 procreating
  & M.delete (-1)
  where
    new = M.mapKeys pred old
    procreating = M.lookup (-1) new & fromMaybe 0

run :: Int -> [Int] -> Integer
run n = sum . M.elems . last . take (succ n) . iterate simulate . startingFish

day6part1 :: String -> String
day6part1 = show . run 80 . parseInput

day6part2 :: String -> String
day6part2 = show . run 256 . parseInput
