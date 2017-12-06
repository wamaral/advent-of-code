module Day3 where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List.Split            (splitWhen)
import           Network.Wreq               (get, responseBody)

readAsInt :: String -> Int
readAsInt x = read x :: Int

layers :: [Int]
layers = foldl (\acc x -> acc ++ [x * 4 - 4 + last acc]) [1] [3,5..999]

-- That's cheating, right?
getOeis :: IO [Int]
getOeis = do
  res <- get "https://oeis.org/A141481/b141481.txt"
  let body = res ^. responseBody
  return $ map (readAsInt . last . words) $ take 100 $ drop 2 $ lines $ unpack body

main :: IO ()
main = do
  let input = 265149
  let layerSteps = length $ takeWhile (< input) layers
  let currentOffset = input - layers !! (layerSteps - 1)
  let distanceFromAxis = currentOffset `mod` layerSteps
  oeis <- getOeis
  putStr "1. "
  print $ layerSteps + distanceFromAxis
  putStr "2. "
  print $ head $ last $ splitWhen (< input) oeis
