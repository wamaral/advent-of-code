module Day3 where

import           Data.List       (all, permutations, transpose)
import           Data.List.Split (chunksOf)

-- Types
data Triangle = Triangle Integer Integer Integer
  deriving Show

-- Logic
validTriangle :: Triangle -> Bool
validTriangle (Triangle a b c) = all (\(x:y:z:_) -> x + y > z) $ permutations [a, b, c]

-- Parse
makeTriangle :: String -> Triangle
makeTriangle line = Triangle a b c
  where
    [a, b, c] = take 3 $ map readInt (words line)
    readInt x = read x :: Integer

-- Main
main :: IO ()
main = do
  inputs <- readFile "input/3"
  let fullData = concatMap words $ lines inputs
  let input1 = chunksOf 3 fullData
  let input2 = chunksOf 3 $ concat $ transpose input1
  putStr "1. "
  putStrLn $ show $ length $ validTriangles input1
  putStr "2. "
  putStrLn $ show $ length $ validTriangles input2
  where
    validTriangles input = filter validTriangle $ map (makeTriangle . unwords) input
