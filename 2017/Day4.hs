module Day4 where

import           Data.List (sort)
import           Data.Set  (fromList, size)

nonDuplicate :: Ord a => [a] -> Bool
nonDuplicate xs = length xs == size (fromList xs)

main :: IO ()
main = do
  input <- lines <$> readFile "input/4"
  putStr "1. "
  print $ length $ filter (nonDuplicate . words) input
  putStr "2. "
  print $ length $ filter (nonDuplicate . map sort . words) input
