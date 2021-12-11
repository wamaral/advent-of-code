module Day11
  (day11part1, day11part2)
  where

import           Common
import           Data.Bifunctor
import           Data.Function
import qualified Data.Map             as M
import           Data.Maybe
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Cave = M.Map (V2 Int) Int

caveParser :: Parser Cave
caveParser = listOfListsToV2Map <$> someTill (someTill charAsInt newline) eof
  where charAsInt = stringToInt0 . (:[]) <$> digitChar

parseInput :: String -> Cave
parseInput = fromMaybe M.empty . parseMaybe caveParser

adjacentDirections :: [V2 Int]
adjacentDirections = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                      V2 (-1) 0,               V2 1 0,
                      V2 (-1) 1,    V2 0 1,    V2 1 1]

adjacents :: V2 Int -> [V2 Int]
adjacents p = map (+ p) adjacentDirections

step :: Cave -> (Cave, Int)
step cave = go firstStep (flashes firstStep)
  & resetCave
  & (\a -> (a, M.filter (== 0) a & M.size))
  where
    firstStep = M.map succ cave
    flashes c = M.filter (> 9) c & M.keys
    resetCave = M.map (\x -> if x == (-1) then 0 else x)
    burnFlashes = M.map (\x -> if x > 9 then -1 else x)
    succ' n = if n == (-1) then n else succ n
    go cv flashed = if null flashed then cv else do
      let toInc = concatMap adjacents flashed & (++ flashed)
      let newCave = foldr (M.adjust succ') (burnFlashes cv) toInc
      go newCave (flashes newCave)

steps :: Int -> Cave -> (Cave, Int)
steps n cave = iterate go (cave, 0) !! max 0 n
  where go (c, i) = step c & second (i +)

day11part1 :: String -> String
day11part1 = show . snd . steps 100 . parseInput

day11part2 :: String -> String
day11part2 _ = ""
