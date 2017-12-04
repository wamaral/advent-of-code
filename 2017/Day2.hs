module Day2 where

import           Data.List (sort)

readAsInteger :: String -> Integer
readAsInteger x = read x :: Integer

-- partial function ¯\_(ツ)_/¯
firstAndLast :: [a] -> (a, a)
firstAndLast xs = (head xs, last xs)

main :: IO ()
main = do
  input <- map (map readAsInteger . words) . lines <$> readFile "input/2"
  putStr "1. "
  print $ sum $ map (uncurry (flip (-)) . firstAndLast . sort) input
  -- putStr "2. "
  -- print ""
