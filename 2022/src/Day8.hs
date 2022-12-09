module Day8
  (day8part1, day8part2)
  where

import           Common
import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Linear.V2
import           Safe
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

isVisibleFromOutside :: Forest -> Tree -> Bool
isVisibleFromOutside forest tree = lineOfSight (forestBoundary forest) tree
  & map (map (forest M.!))
  & any (all (< treeSize))
  where treeSize = (M.!) forest tree

canSeeTrees :: Forest -> Tree -> Int
canSeeTrees forest tree = canSee up * canSee down * canSee left * canSee right
  where
    treeX = tree ^._x
    treeY = tree ^._y
    boundary = forestBoundary forest
    boundaryX = boundary ^._x
    boundaryY = boundary ^._y
    left = map (`V2` treeY) [treeX,treeX-1..0] & tail
    right = map (`V2` treeY) [treeX..boundaryX] & tail
    up = map (V2 treeX) [treeY,treeY-1..0] & tail
    down = map (V2 treeX) [treeY..boundaryY] & tail
    treeSize = (M.!) forest
    thisTreeSize = treeSize tree
    canSee direction = (\(a,b) -> length a + (if headDef 0 b >= thisTreeSize then 1 else 0)) $ span (< thisTreeSize) (map treeSize direction)

day8part1 :: String -> String
day8part1 input = forest
  & M.keys
  & filter (isVisibleFromOutside forest)
  & length
  & show
  where forest = parseInput input

day8part2 :: String -> String
day8part2 input = forest
  & M.keys
  & map (canSeeTrees forest)
  & maximum
  & show
  where forest = parseInput input
