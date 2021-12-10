module Day9
  (day9part1, day9part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Cave = M.Map (V2 Int) Int

caveParser :: Parser Cave
caveParser = listOfListsToV2Map <$> someTill (someTill charAsInt newline) eof
  where charAsInt = stringToInt0 . (:[]) <$> digitChar

parseInput :: String -> Cave
parseInput = fromMaybe M.empty . parseMaybe caveParser

adjacents :: V2 Int -> [V2 Int]
adjacents p = map (+ p) [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]

isLowPoint :: Cave -> V2 Int -> Int -> Bool
isLowPoint cave p h = all (> h) $ mapMaybe (`M.lookup` cave) (adjacents p)

basin :: Cave -> V2 Int -> Int
basin cave p = adjInBasin S.empty (S.fromList [p]) & S.size
  where
    pointInBasin = (< 9) . fromMaybe 9 . (`M.lookup` cave)
    adjInBasin inBasin toCheck = if S.null toCheck then inBasin else do
      let newInBasin = S.toList toCheck
                         & concatMap adjacents
                         & S.fromList
                         & (`S.union` toCheck)
                         & S.filter (`S.notMember` inBasin)
                         & S.filter pointInBasin
      adjInBasin (S.union inBasin newInBasin) newInBasin

day9part1 :: String -> String
day9part1 input = M.filterWithKey (isLowPoint cave) cave
  & M.foldr (\x acc -> acc + succ x) 0
  & show
  where cave = parseInput input

day9part2 :: String -> String
day9part2 input = M.filterWithKey (isLowPoint cave) cave
  & M.keys
  & map (basin cave)
  & sortBy (flip compare)
  & take 3
  & product
  & show
  where cave = parseInput input
