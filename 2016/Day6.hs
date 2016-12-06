module Day6 where

import           Data.List (group, sort, sortOn, transpose)

-- Main
main :: IO ()
main = do
  input <- readFile "input/6"
  putStr "1. "
  putStrLn $ map (mapfn reverse) $ transpose $ lines input
  putStr "2. "
  putStrLn $ map (mapfn id) $ transpose $ lines input
  where
    mapfn f = (head . head . f . (sortOn length) . group . sort)
