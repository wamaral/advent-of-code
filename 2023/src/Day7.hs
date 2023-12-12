{-# LANGUAGE RecordWildCards #-}

module Day7
  (day7part1, day7part2)
  where

import Common
import Data.Function
import Data.List (elemIndex, group, sort)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data HandType = Five | Four | Full | Three | TwoPair | Two | High deriving (Eq, Ord, Show)
data Game = Game { hand :: Hand, bid :: Int, handType :: HandType, cardOrders :: [Int] } deriving Show
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

getType :: Hand -> HandType
getType h
  | any ((== 5) . length) $ group $ sort h = Five
  | any ((== 4) . length) $ group $ sort h = Four
  | (== [2,3]) $ sort $ map length $ group $ sort h = Full
  | any ((== 3) . length) $ group $ sort h = Three
  | (== [1,2,2]) $ sort $ map length $ group $ sort h = TwoPair
  | any ((== 2) . length) $ group $ sort h = Two
  | otherwise = High

gameParser :: Parser Game
gameParser = do
  hand <- count 5 (oneOf cardList)
  _ <- hspace
  bid <- intParser
  let handType = getType hand
  let cardOrders = map (\c -> fromMaybe 99 $ elemIndex c cardList) hand
  pure Game{..}

day7part1 :: String -> String
day7part1 input = readListOf gameParser input
  & sort
  & reverse
  & zipWith (\i g -> i * bid g) [1 ..]
  & sum
  & show

day7part2 :: String -> String
day7part2 _ = ""
