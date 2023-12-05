{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Day4
  (day4part1, day4part2)
  where

import           Common
import           Data.Function
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Card = Card
  { cardId :: Int
  , winningNumbers :: S.IntSet
  , cardNumbers :: S.IntSet
  } deriving Show

type CardPile = M.IntMap Int

newPile :: Int -> CardPile
newPile size = M.fromDistinctAscList $ fmap (,1) [1..size]

cardParser :: Parser Card
cardParser = do
  _ <- string "Card"
  _ <- hspace
  cardId <- intParser
  _ <- char ':'
  _ <- hspace
  winningNumbers <- S.fromList <$> many (intParser <* optional hspace)
  _ <- char '|'
  _ <- hspace
  cardNumbers <- S.fromList <$> many (intParser <* optional hspace)
  _ <- optional newline
  pure Card{..}

cardValue :: Card -> Int
cardValue card = cardValue2 card
  & (\n -> if n == 0 then 0 else 2 ^ (n - 1))

cardValue2 :: Card -> Int
cardValue2 card = S.intersection (winningNumbers card) (cardNumbers card)
  & S.size

checkCard :: CardPile -> Card -> CardPile
checkCard pile card = map (+ cardId card) [0..value]
  & tail
  & foldr (M.adjust (+ cardsNow)) pile
  where
    value = cardValue2 card
    cardsNow = M.findWithDefault 0 (cardId card) pile

day4part1 :: String -> String
day4part1 = show . sum . map cardValue . readListOf cardParser

day4part2 :: String -> String
day4part2 input = cards
  & foldl checkCard pile
  & M.elems
  & sum
  & show
  where
    cards = readListOf cardParser input
    pile = newPile $ length cards
