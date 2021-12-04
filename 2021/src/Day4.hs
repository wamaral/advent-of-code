{-# LANGUAGE RecordWildCards #-}

module Day4
  (day4part1, day4part2)
  where

import           Common
import           Control.Lens.Operators
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Draw = [Int]
type Board = M.Map (V2 Int) Int
data Input = Input {draw :: Draw, boards :: [Board]} deriving Show

drawParser :: Parser Draw
drawParser = intParser `sepBy` char ','

boardLineParser :: Parser [Int]
boardLineParser = hspace *> some (intParser <* hspace)

boardParser :: Parser Board
boardParser = listOfListsToV2Map <$> some (boardLineParser <* newline)

inputParser :: Parser Input
inputParser = do
  draw <- drawParser <* newline
  _ <- newline
  boards <- some (boardParser <* optional newline)
  _ <- eof
  return Input{..}

parseInput :: String -> Input
parseInput = fromMaybe Input{draw = [], boards = []} . parseMaybe inputParser

isWinner :: Draw -> Board -> Bool
isWinner draw board = anyLineWin || anyColWin
  where
    maxVec = fst $ M.findMax board
    maxCols = maxVec ^._x
    maxLines = maxVec ^._y
    isLineWin n = all ((`elem` draw) . (\x -> fromMaybe (-1) $ M.lookup (V2 x n) board)) [0..maxCols]
    isColWin n = all ((`elem` draw) . (\y -> fromMaybe (-1) $ M.lookup (V2 n y) board)) [0..maxLines]
    anyLineWin = any isLineWin [0..maxLines]
    anyColWin = any isColWin [0..maxCols]

getWinner :: Input -> (Board, Draw)
getWinner (Input draw boards) = go 0
  where go n = do
          let partialDraw = take n draw
          case find (isWinner partialDraw) boards of
            Nothing -> go (succ n)
            Just w  -> (w, partialDraw)

getLastWinner :: Input -> (Board, Draw)
getLastWinner (Input draw boards) = go boards 0
  where go bs n = do
          let partialDraw = take n draw
          let (winners, losers) = partition (isWinner partialDraw) bs
          if null losers && length winners == 1
            then (head winners, partialDraw)
            else go losers (succ n)

getUnmarked :: Board -> Draw -> Board
getUnmarked board draw = M.filter (`notElem` draw) board

day4part1 :: String -> String
day4part1 input = do
  let (winner, draw) = getWinner $ parseInput input
  let unmarked = getUnmarked winner draw
  show $ sum (M.elems unmarked) * last draw

day4part2 :: String -> String
day4part2 input = do
  let (lastWinner, draw) = getLastWinner $ parseInput input
  let unmarked = getUnmarked lastWinner draw
  show $ sum (M.elems unmarked) * last draw
