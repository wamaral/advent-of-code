module Day6 where

import           Data.List (group, sort, sortOn, transpose)

-- Main
main :: IO ()
main = do
  input <- readFile "input/6"
  putStr "1. "
  putStrLn $ map (head . head . reverse . (sortOn length) . group . sort) $ transpose $ lines input
  putStr "2. "
  putStrLn "TODO"
