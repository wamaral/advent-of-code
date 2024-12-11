module Day4
  (day4part1, day4part2)
  where

import           Common
import           Data.Function
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Linear.V2

type WordSearch = M.Map (V2 Int) Char

directions1 :: [[V2 Int]]
directions1 =
  [ map (\n -> V2 0 n) [0..3]
  , map (\n -> V2 n 0) [0..3]
  , map (\n -> V2 0 (- n)) [0..3]
  , map (\n -> V2 (- n) 0) [0..3]
  , map (\n -> V2 n n) [0..3]
  , map (\n -> V2 (- n) n) [0..3]
  , map (\n -> V2 n (-n)) [0..3]
  , map (\n -> V2 (- n) (-n)) [0..3]
  ]

directions2 :: [[V2 Int]]
directions2 =
  [ [V2 (-1) (-1), V2 0 0, V2 1 1]
  , [V2 (-1) 1, V2 0 0, V2 1 (-1)]
  ]

possibleXmas :: [[V2 Int]] -> V2 Int -> [[V2 Int]]
possibleXmas ds p = (map . map) (+ p) ds

possibleWords :: [[V2 Int]] -> WordSearch -> V2 Int -> [String]
possibleWords ds search p = possibleXmas ds p
  & map (getWord search)

getWord :: WordSearch -> [V2 Int] -> String
getWord search = mapMaybe (`M.lookup` search)

day4part1 :: String -> String
day4part1 input = M.filter (== 'X') search
  & M.keys
  & concatMap (possibleWords directions1 search)
  & filter (== "XMAS")
  & length
  & show
  where search = listOfListsToV2Map $ lines input

day4part2 :: String -> String
day4part2 input = M.filter (== 'A') search
  & M.keys
  & map (possibleWords directions2 search)
  & filter (all (`elem` ["MAS", "SAM"]))
  & length
  & show
  where search = listOfListsToV2Map $ lines input
