module Day3 where

import           Data.List (all, permutations)

data Triangle = Triangle Integer Integer Integer
  deriving Show

validTriangle :: Triangle -> Bool
validTriangle (Triangle a b c) = all (\(x:y:z:_) -> x + y > z) $ permutations [a, b, c]

makeTriangle :: String -> Triangle
makeTriangle line = Triangle a b c
  where
    [a, b, c] = take 3 $ map readInt (words line)
    readInt x = read x :: Integer

-- Main
main :: IO ()
main = do
  inputs <- readFile "input/3"
  putStr "1. "
  putStrLn $ show $ length $ filter validTriangle $ map makeTriangle $ lines inputs
  putStr "2. "
  putStrLn "TODO"
