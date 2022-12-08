module Day8
  (day8part1, day8part2)
  where

import           Common
import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Linear.V2
import           Text.Megaparsec

type Tree = V2 Int
type Forest = M.Map Tree Int

forestParser :: Parser Forest
forestParser = listOfListsToV2Map <$> linesParser (some singleDigitParser)

parseInput :: String -> Forest
parseInput = fromMaybe M.empty . parseMaybe forestParser

forestBoundary :: Forest -> Tree
forestBoundary = fst . M.findMax

lineOfSight :: Tree -> Tree -> [[Tree]]
lineOfSight boundary tree = map (filter (/= tree)) [left, right, up, down]
  where
    left = map (`V2` (tree ^._y)) [0..(tree ^._x)]
    right = map (`V2` (tree ^._y)) [(tree ^._x)..(boundary ^._x)]
    up = map (V2 (tree ^._x)) [0..(tree ^._y)]
    down = map (V2 (tree ^._x)) [(tree ^._y)..(boundary ^._y)]

isVisible :: Forest -> Tree -> Bool
isVisible forest tree = lineOfSight (forestBoundary forest) tree
  & map (map (forest M.!))
  & any (all (< treeSize))
  where treeSize = (M.!) forest tree

day8part1 :: String -> String
day8part1 input = forest
  & M.keys
  & filter (isVisible forest)
  & length
  & show
  where forest = parseInput input

day8part2 :: String -> String
day8part2 _ = ""
