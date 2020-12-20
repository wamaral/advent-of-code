module Day19
  (day19part1, day19part2)
  where

import           Common
import           Data.Function
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Message = String

type Index = Int

type Ruleset = M.Map Index Rule

data Rule = Token Char
  | Product [Index]
  | Sum [Index] [Index]
  deriving Show

ruleParser :: Parser (Index, Rule)
ruleParser = do
  index <- intParser
  _ <- string ": "
  rule <- choice
    [ Token <$> between (char '"') (char '"') letterChar
    , try sumParser
    , productParser
    ]
  _ <- newline
  return (index, rule)
  where
    spacedIntParser = many (intParser <* optional (char ' '))
    productParser = Product <$> spacedIntParser
    sumParser = Sum <$> spacedIntParser <*> (string "| " *> spacedIntParser)

inputParser :: Parser (Ruleset, [Message])
inputParser = do
  rules <- many ruleParser
  _ <- newline
  messages <- many letterChar `sepBy` newline
  _ <- eof
  return (M.fromList rules, messages)

parseInput :: String -> (Ruleset, [Message])
parseInput = fromMaybe (M.empty, []) . parseMaybe inputParser

allValidMessages :: Ruleset -> Index -> [Message]
allValidMessages rules i = case M.lookup i rules of
  Nothing           -> []
  Just (Token x)    -> [[x]]
  Just (Product xs) -> subMessages xs
  Just (Sum x1 x2)  -> subMessages x1 ++ subMessages x2
  where
    subMessages idxs = map (allValidMessages rules) idxs & sequence & map concat

day19part1 :: String -> String
day19part1 input = filter (`S.member` validMessages) messages & length & show
  where
    (ruleset, messages) = parseInput input
    validMessages = allValidMessages ruleset 0 & S.fromList

day19part2 :: String -> String
day19part2 _ = ""
