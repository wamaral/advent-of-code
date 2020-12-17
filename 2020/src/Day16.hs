{-# LANGUAGE RecordWildCards #-}

module Day16
  (day16part1, day16part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Rule = Rule
  { ruleName :: String
  , range    :: [Int]
  } deriving Show

instance Eq Rule where
  r1 == r2 = ruleName r1 == ruleName r2

type Ticket = [Int]

data FieldMatch = FieldMatch
  { index  :: Int
  , values :: [Int]
  , rule   :: Maybe Rule
  } deriving Show

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

validTicket :: [Rule] -> Ticket -> Bool
validTicket rules ticket = invalidFields rules ticket & length & (== 0)

allValuesMatch :: Rule -> [Int] -> Bool
allValuesMatch rule = all (`elem` range rule)

guessTheField :: [Rule] -> [Int] -> [Rule]
guessTheField rules xs = filter (`allValuesMatch` xs) rules

shrink :: [FieldMatch] -> [Rule] -> [FieldMatch]
shrink [] _ = []
shrink fms rules = matched ++ shrink unmatched remainingRules
  where
    tryMatch fm = fm {rule = guessTheField rules (values fm) & (\rs -> if length rs == 1 then Just (head rs) else Nothing)}
    applied = map tryMatch fms
    (matched, unmatched) = partition (isJust . rule) applied
    remainingRules = rules \\ mapMaybe rule matched

day16part1 :: String -> String
day16part1 input = show $ sum $ concatMap (invalidFields rules) nearbyTickets
  where (rules, _, nearbyTickets) = parseInput input

day16part2 :: String -> String
day16part2 input = show $ product $ map (atDef 0 myTicket . index) $ filter isDeparture $ shrink fieldMatches rules
  where
    (rules, myTicket, nearbyTickets) = parseInput input
    allValidTickets = (myTicket : nearbyTickets) & filter (validTicket rules)
    fieldMatches = zipWith (\i xs -> FieldMatch {index = i, values = xs, rule = Nothing}) [0 ..] (transpose allValidTickets)
    isDeparture fm = rule fm & fmap (isPrefixOf "departure" . ruleName) & fromMaybe False
