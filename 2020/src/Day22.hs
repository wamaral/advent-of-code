module Day22
  (day22part1, day22part2)
  where

import           Common
import           Control.Monad.State
import           Data.Maybe
import           Safe
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

type Card = Int
type Hand = [Card]
type Game = (Hand, Hand)
type GameList = [Game]

handParser :: Parser Hand
handParser = do
  _ <- string "Player " *> intParser *> char ':' *> newline
  many (intParser <* newline)

gameParser :: Parser (Hand, Hand)
gameParser = do
  h1 <- handParser
  _ <- newline
  h2 <- handParser
  _ <- eof
  return (h1, h2)

parseInput :: String -> Game
parseInput = fromMaybe ([], []) . parseMaybe gameParser

runGame :: State Game Hand
runGame = do
  (h1, h2) <- get
  if null h1 then return h2 else if null h2 then return h1 else do
    let card1 = headDef 0 h1
    let card2 = headDef 0 h2
    let h1' = tailDef [] h1
    let h2' = tailDef [] h2
    put $ if card1 >= card2 then (h1' ++ [card1, card2], h2') else (h1', h2' ++ [card2, card1])
    runGame

runGame2 :: State GameList Hand
runGame2 = do
  allStates <- get
  let (h1, h2) = headDef ([], []) allStates
  if (h1, h2) `elem` tailDef [] allStates || null h2 then return h1 else if null h1 then return h2 else do
      let card1 = headDef 0 h1
      let card2 = headDef 0 h2
      let h1' = tailDef [] h1
      let h2' = tailDef [] h2
      put $ (: allStates)  $ if card1 > length h1' || card2 > length h2'
        then if card1 >= card2 then (h1' ++ [card1, card2], h2') else (h1', h2' ++ [card2, card1])
        else do
          let (subh1, _subh2) = headDef ([], []) $ execState runGame2 [(take card1 h1', take card2 h2')]
          if null subh1 then (h1', h2' ++ [card2, card1]) else (h1' ++ [card1, card2], h2')
      runGame2

day22part1 :: String -> String
day22part1 = show . sum . zipWith (*) [1..] . reverse . evalState runGame . parseInput

day22part2 :: String -> String
day22part2 = show . sum . zipWith (*) [1..] . reverse . evalState runGame2 . (: []) . parseInput
