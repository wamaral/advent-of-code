module Day14
  (day14part1, day14part2)
  where

import           Common
import           Data.Function
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Polymer = M.Map String Integer
type Rules = M.Map String Char

ruleParser :: Parser (String, Char)
ruleParser = do
  pair <- some upperChar
  _ <- string " -> "
  c <- upperChar
  return (pair, c)

rulesParser :: Parser Rules
rulesParser = M.fromList <$> some (ruleParser <* optional newline)

inputParser :: Parser (Polymer, String, Rules)
inputParser = do
  polymerString <- some (upperChar <* optional newline)
  let polymer = M.fromListWith (+) $ map (\(a,b) -> ([a,b], 1)) $ zipTail polymerString
  _ <- newline
  rules <- rulesParser
  _ <- eof
  return (polymer, polymerString, rules)

parseInput :: String -> (Polymer, String, Rules)
parseInput = fromMaybe (M.empty, "", M.empty) . parseMaybe inputParser

step :: Rules -> Polymer -> Polymer
step rules p = M.foldrWithKey updatePolymer p rules
  where updatePolymer ab c polymer = case M.lookup ab p of
          Nothing -> polymer
          Just n -> M.insertWith (+) [head ab,c] n polymer
            & M.insertWith (+) [c,last ab] n
            & M.adjust (\x -> x - n) ab

runTimes :: Int -> (Polymer, String, Rules) -> Integer
runTimes n (polymer, polymerString, rules) = polymer
  & iterate (step rules)
  & take (succ n)
  & last
  & M.mapKeysWith (+) head
  & M.insertWith (+) (last polymerString) 1
  & M.elems
  & (\xs -> maximum xs - minimum xs)

day14part1 :: String -> String
day14part1 = show . runTimes 10 . parseInput

day14part2 :: String -> String
day14part2 = show . runTimes 40 . parseInput
