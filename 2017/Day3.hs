module Day3 where

import           Data.List (sort)

layers :: [Int]
layers = foldl (\acc x -> acc ++ [x * 4 - 4 + last acc]) [1] [3,5..999]

foo limit x = if (x - limit) < 0 then x else foo limit (x - limit)

main :: IO ()
main = do
  let input = 265149
  let layerSteps = length $ takeWhile (< input) layers
  let currentOffset = input - layers !! (layerSteps - 1)
  let distanceFromAxis = currentOffset `mod` layerSteps
  putStr "1. "
  print $ layerSteps + distanceFromAxis
  -- putStr "2. "
  -- print ""
