module Main where

import           Common
import qualified Data.Map.Strict as Map
import           Day1
import           Day10
import           Day11
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           System.IO

completeParts :: Map.Map Int [String -> String]
completeParts = Map.fromList
  [ (1, [day1part1, day1part2])
  , (2, [day2part1, day2part2])
  , (3, [day3part1, day3part2])
  , (4, [day4part1, day4part2])
  , (5, [day5part1, day5part2])
  , (6, [day6part1, day6part2])
  , (7, [day7part1, day7part2])
  , (8, [day8part1, day8part2])
  , (9, [day9part1, day9part2])
  , (10, [day10part1, day10part2])
  , (11, [day11part1, day11part2])
  ]

main :: IO ()
main = do
  putStr "Choose day to run: "
  hFlush stdout

  chosenDay <- stringToInt <$> getLine
  input <- maybe (pure "" :: IO String) readInput chosenDay

  case chosenDay >>= flip Map.lookup completeParts of
    Nothing       -> putStrLn "Day not found"
    Just dayParts -> mapM_ (putStrLn . (\f -> f input)) dayParts
