{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Day21
  (day21part1, day21part2)
  where

import           Common
import           Control.Lens         hiding (element)
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Turn = Player1 | Player2 deriving (Eq, Show)

instance Enum Turn where
  toEnum n = cycle [Player2, Player1] !! n
  fromEnum t = if t == Player1 then 1 else 2

data Player = Player
  { score    :: Int
  , position :: Int
  }

instance Show Player where
  show p = show (score p) ++ " @" ++ show (position p)

data Game = Game
  { _player1 :: Player
  , _player2 :: Player
  , die      :: [Int]
  , rolls    :: Int
  , nextTurn :: Turn
  }
$(makeLenses ''Game)

instance Show Game where
  show g = "<" ++ show (_player1 g)
    ++ " <=> " ++ show (_player2 g)
    ++ "> :: next turn: " ++ show (nextTurn g)
    ++ " :: next roll: " ++ show (headDef 0 $ die g)
    ++ " :: total rolls: " ++ show (rolls g)

initGame :: Int -> Int -> Game
initGame position1 position2 = Game {die = cycle [1..100], rolls = 0, nextTurn = Player1, ..}
  where
    _player1 = Player {score = 0, position = position1}
    _player2 = Player {score = 0, position = position2}

inputParser :: Parser Game
inputParser = do
  p1 <- string "Player 1 starting position: " *> (intParser <* newline)
  p2 <- string "Player 2 starting position: " *> (intParser <* optional newline)
  return $ initGame p1 p2

parseInput :: String -> Game
parseInput = fromMaybe (initGame 0 0) . parseMaybe inputParser

walk :: Int -> Int -> Int
walk steps currentPos = cycle [1..10] !! pred (steps + currentPos)

playerRoll :: [Int] -> Player -> Player
playerRoll rs p = p {position = walked, score = walked + score p}
  where walked = walk (sum rs) (position p)

play :: Game -> Game
play g = do
  let curPlayer = if nextTurn g == Player1 then player1 else player2
  let (roll, newDie) = splitAt 3 (die g)
  g {die = newDie, nextTurn = succ (nextTurn g), rolls = 3 + rolls g} & over curPlayer (playerRoll roll)

isGameOver :: Game -> Bool
isGameOver g = any (>= 1000) [g & _player1 & score, g & _player2 & score]

gameResult :: Game -> Int
gameResult g = minimum [g & _player1 & score, g & _player2 & score] * rolls g

day21part1 :: String -> String
day21part1 = show . gameResult . head . dropWhile (not . isGameOver) . iterate play . parseInput

day21part2 :: String -> String
day21part2 _ = ""
