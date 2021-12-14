module Day14
  (day14part1, day14part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Polymer = String
type Rules = M.Map String Char

ruleParser :: Parser (String, Char)
ruleParser = do
  pair <- some upperChar
  _ <- string " -> "
  c <- upperChar
  return (pair, c)

rulesParser :: Parser Rules
rulesParser = M.fromList <$> some (ruleParser <* optional newline)

inputParser :: Parser (Polymer, Rules)
inputParser = do
  polymer <- some (upperChar <* optional newline)
  _ <- newline
  rules <- rulesParser
  _ <- eof
  return (polymer, rules)

parseInput :: String -> (Polymer, Rules)
parseInput = fromMaybe ("", M.empty) . parseMaybe inputParser

step :: Rules -> Polymer -> Polymer
step rules p = zipTail p & concatMap applyRule & (head p :)
  where applyRule (a,b) = maybe [b] (\c -> [c,b]) $ M.lookup [a,b] rules

day14part1 :: String -> String
day14part1 input = polymer
  & iterate (step rules)
  & take 11
  & last
  & sort
  & group
  & (\xs -> (maximumBy (compare `on` length) xs, minimumBy (compare `on` length) xs))
  & (\(a,b) -> length a - length b)
  & show
  where (polymer, rules) = parseInput input

day14part2 :: String -> String
day14part2 _ = ""
