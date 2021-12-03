module Day3
  (day3part1, day3part2)
  where

import           Common
import           Control.Applicative
import           Data.Char
import           Data.Function
import           Data.List
import           Safe
import           Text.Megaparsec.Char

bitstringToInt :: String -> Int
bitstringToInt = sum . zipWith fromBit [0 .. ] . reverse . map digitToInt
  where
    fromBit :: Int -> Int -> Int
    fromBit index bitVal = 2 ^ index * bitVal

reportParser :: Parser String
reportParser = many binDigitChar

majority :: String -> Char
majority = headDef '1' . maximumBy (compare `on` length) . group . sort

minority :: String -> Char
minority = headDef '0' . minimumBy (compare `on` length) . group . sort

flipBits :: String -> String
flipBits = map (\x -> if x == '1' then '0' else '1')

commonBy :: (String -> Char) -> [String] -> String
commonBy f = go 0
  where go i ys = do
          let filterBy = f $ atDef [] (transpose ys) i
          let filteredVals = filter (\y -> atDef 'X' y i == filterBy) ys
          if length filteredVals == 1 then head filteredVals else go (i + 1) filteredVals

day3part1 :: String -> String
day3part1 = show . uncurry (*) . bothValues . map majority . transpose . readListOf reportParser
  where bothValues x = (bitstringToInt x, bitstringToInt $ flipBits x)

day3part2 :: String -> String
day3part2 = show . uncurry (*) . bothValues . readListOf reportParser
  where bothValues x = (bitstringToInt $ commonBy majority x, bitstringToInt $ commonBy minority x)
