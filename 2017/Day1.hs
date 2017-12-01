module Day1 where

import           Data.List (group)

readCharAsInteger :: Char -> Integer
readCharAsInteger x = read [x] :: Integer

circular :: [a] -> [a]
circular []     = []
circular (x:xs) = x:xs ++ [x]

-- Main
main :: IO ()
main = do
  input <- map readCharAsInteger . head . lines <$> readFile "input/1"
  putStr "1. "
  print $ sum $ concatMap tail $ filter (\x -> length x > 1) $ group $ circular input
  putStr "2. "
  print $ sum $ map (uncurry (+)) $ filter (uncurry (==)) $ uncurry zip $ splitAt ((length input + 1) `div` 2) input
