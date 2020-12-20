module Day19
  (day19part1, day19part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
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

matchAllRules :: Ruleset -> Message -> Bool
matchAllRules rules = any null . matchRule 0
  where
    matchRule :: Index -> Message -> [Message]
    matchRule ruleNumber message = case M.lookup ruleNumber rules of
      Nothing           -> []
      Just (Token c)    -> [tail message | [c] `isPrefixOf` message]
      Just (Product xs) -> subRules message [xs]
      Just (Sum xs ys)  -> subRules message [xs, ys]
    subRules :: Message -> [[Index]] -> [Message]
    subRules message idxs = concatMap (foldl' (flip (concatMap . matchRule)) [message]) idxs

day19part1 :: String -> String
day19part1 input = filter (matchAllRules ruleset) messages & length & show
  where
    (ruleset, messages) = parseInput input

day19part2 :: String -> String
day19part2 input = filter (matchAllRules updatedRuleset) messages & length & show
  where
    (ruleset, messages) = parseInput input
    updatedRuleset = M.insert 8 (Sum [42] [42, 8]) $ M.insert 11 (Sum [42, 31] [42, 11, 31]) ruleset
