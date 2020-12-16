{-# LANGUAGE RecordWildCards #-}

module Day16
  (day16part1, day16part2)
  where

import           Common
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Rule = Rule
  { ruleName :: String
  , range    :: [Int]
  } deriving Show

type Ticket = [Int]

ruleParser :: Parser Rule
ruleParser = do
  ruleName <- many (letterChar <|> spaceChar)
  _ <- string ": "
  (a1, b1) <- rangeParser
  _ <- string " or "
  (a2, b2) <- rangeParser
  let range = [a1..b1] ++ [a2..b2]
  return Rule{..}
  where
    rangeParser :: Parser (Int, Int)
    rangeParser = (,) <$> intParser <*> (char '-' *> intParser)

ticketParser :: Parser Ticket
ticketParser = intParser `sepBy` char ','

inputParser :: Parser ([Rule], Ticket, [Ticket])
inputParser = do
  rules <- manyTill (ruleParser <* newline) newline
  _ <- string "your ticket:"
  _ <- newline
  myTicket <- ticketParser <* newline
  _ <- newline
  _ <- string "nearby tickets:"
  _ <- newline
  nearbyTickets <- manyTill (ticketParser <* newline) eof
  return (rules, myTicket, nearbyTickets)

parseInput :: String -> ([Rule], Ticket, [Ticket])
parseInput input = fromMaybe ([], [], [[]]) $ parseMaybe inputParser input

invalidFields :: [Rule] -> Ticket -> [Int]
invalidFields rules = filter outsideAllRanges
  where outsideAllRanges x = all (notElem x . range) rules

day16part1 :: String -> String
day16part1 input = show $ sum $ concatMap (invalidFields rules) nearbyTickets
  where (rules, _, nearbyTickets) = parseInput input

day16part2 :: String -> String
day16part2 _ = ""
