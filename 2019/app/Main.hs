module Main where

import           Common
import qualified Data.Map.Strict as Map
import           Day1
import           Day2
import           Day3
import           System.IO

completeParts :: Map.Map Int [String -> String]
completeParts = Map.fromList
  [ (1, [day1part1, day1part2])
  , (2, [day2part1, day2part2])
  , (3, [day3part1, day3part2])
  ]

main :: IO ()
main = do
  let dayCount = length completeParts

  putStr $ "Choose day to run (1-" ++ show dayCount ++ "): "
  hFlush stdout

  chosenDay <- stringToInt <$> getLine
  input <- maybe (pure "" :: IO String) readInput chosenDay

  case chosenDay >>= flip Map.lookup completeParts of
    Nothing       -> putStrLn "Day not found"
    Just dayParts -> mapM_ (putStrLn . (\f -> f input)) dayParts
