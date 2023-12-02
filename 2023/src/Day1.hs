module Day1
  (day1part1, day1part2)
  where

import Common
import Data.Char (intToDigit, isDigit)
import Data.List (isPrefixOf, findIndex, tails)
import Data.Maybe (fromMaybe)

firstLast :: [a] -> [a]
firstLast [] = []
firstLast xs = [head xs, last xs]

numbers :: [String]
numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

textToNumber :: String -> Char
textToNumber s = fromMaybe (head s) $ fmap intToDigit $ findIndex (`isPrefixOf` s) numbers

getNumbers :: String -> String
getNumbers [] = []
getNumbers xs = map textToNumber $ filter (not . null) $ tails xs

day1part1 :: String -> String
day1part1 = show . sum . map (stringToInt0 . firstLast . filter isDigit) . lines

day1part2 :: String -> String
day1part2 = show . sum . map (stringToInt0 . firstLast . filter isDigit . getNumbers) . lines
