module Day1
  (day1part1, day1part2)
  where

import           Common

fuelRequired :: Int -> Int
fuelRequired m = maximum [0, (m `div` 3) - 2]

fuelRequiredWithFuel :: Int -> Int
fuelRequiredWithFuel x
  | x <= 0    = x
  | otherwise = required + fuelRequiredWithFuel required
  where required = fuelRequired x

day1part1 :: String -> String
day1part1 = show . sum . map fuelRequired . readListOf intParser

day1part2 :: String -> String
day1part2 = show . sum . map fuelRequiredWithFuel . readListOf intParser
