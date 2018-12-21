module Main where

import           Common
import qualified Data.Map.Strict as Map
import           Day1
import           System.IO

completeParts :: Map.Map Int [(String -> String)]
completeParts = Map.fromList
  [(1, [day1part1, day1part2])]

main :: IO ()
main = do
  let dayCount = length completeParts

  putStr $ "Choose day to run (1-" ++ show dayCount ++ ") "
  hFlush stdout

  chosenDay <- stringToInt <$> getLine
  input <- maybe (pure "" :: IO String) readInput chosenDay

  case chosenDay >>= (flip Map.lookup) completeParts of
    Nothing       -> putStrLn "Day not found"
    Just dayParts -> mapM_ putStrLn $ map (\f -> f input) dayParts
