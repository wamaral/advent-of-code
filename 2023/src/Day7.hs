{-# LANGUAGE RecordWildCards #-}

module Day7
  (day7part1, day7part2)
  where

import Common
import Data.Function
import Data.List (elemIndex, group, sort, minimumBy)
import Data.Maybe
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Char

data HandType = Five | Four | Full | Three | TwoPair | Two | High deriving (Eq, Ord, Show)
data Game = Game { hand :: Hand, bid :: Int, handType :: HandType, cardOrders :: [Int], isJoker :: Bool } deriving Show
type Hand = [Card]
type Card = Char

instance Eq Game where
  g1 == g2 = hand g1 == hand g2

instance Ord Game where
  g1 `compare` g2 = if handType g1 /= handType g2
    then handType g1 `compare` handType g2
    else cardOrders g1 `compare` cardOrders g2

cardList :: [Card]
cardList = "AKQJT98765432"

cardListJoker :: [Card]
cardListJoker = "AKQT98765432J"

getType :: Hand -> HandType
getType h = case sort $ map length $ group $ sort h of
  [5]       -> Five
  [1,4]     -> Four
  [2,3]     -> Full
  [1,1,3]   -> Three
  [1,2,2]   -> TwoPair
  [1,1,1,2] -> Two
  _         -> High

gameParser :: Bool -> Parser Game
gameParser isJoker = do
  originalHand <- count 5 (oneOf cardList)
  _ <- hspace
  bid <- intParser
  let hand = if isJoker then bestJoker originalHand else originalHand
  let handType = getType hand
  let clist = if isJoker then cardListJoker else cardList
  let cardOrders = map (\c -> fromMaybe 99 $ elemIndex c clist) originalHand
  pure Game{..}

jokerizer :: Hand -> [Hand]
jokerizer [] = [[]]
jokerizer (c:cs) = concatMap (\c' -> map (c' :) (jokerizer cs)) replacements
  where replacements = if c == 'J' then init cardListJoker else [c]

bestJoker :: Hand -> Hand
bestJoker = minimumBy (comparing getType) . jokerizer

runGames :: [Game] -> String
runGames games = games
  & sort
  & reverse
  & zipWith (\i g -> i * bid g) [1 ..]
  & sum
  & show

day7part1 :: String -> String
day7part1 = runGames . readListOf (gameParser False)

day7part2 :: String -> String
day7part2 = runGames . readListOf (gameParser True)
