{-# LANGUAGE RecordWildCards #-}

module Day4
  (day4part1, day4part2)
  where

import           Common
import           Data.Function
import qualified Data.IntSet as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Card = Card
  { cardId :: Int
  , winningNumbers :: S.IntSet
  , cardNumbers :: S.IntSet
  } deriving Show

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
cardValue card = S.intersection (winningNumbers card) (cardNumbers card)
  & S.size
  & (\n -> if n == 0 then 0 else 2 ^ (n - 1))

day4part1 :: String -> String
day4part1 = show . sum . map cardValue . readListOf cardParser

day4part2 :: String -> String
day4part2 _ = ""
