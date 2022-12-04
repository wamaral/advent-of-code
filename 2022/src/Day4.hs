module Day4
  (day4part1, day4part2)
  where

import           Common
import           Text.Megaparsec.Char

type Assignment = (Int, Int)
type ElfPair = (Assignment, Assignment)

assignmentParser :: Parser Assignment
assignmentParser = (,) <$> (intParser <* char '-') <*> intParser

elfPairParser :: Parser ElfPair
elfPairParser = (,) <$> (assignmentParser <* char ',') <*> assignmentParser

isSubset :: ElfPair -> Bool
isSubset ((a,b),(x,y)) = (a <= x && b >= y) || (x <= a && y >= b)

overlap :: ElfPair -> Bool
overlap ((a,b),(x,y)) = not (x > b || y < a)

day4part1 :: String -> String
day4part1 = show . length . filter isSubset . readListOf elfPairParser

day4part2 :: String -> String
day4part2 = show . length . filter overlap . readListOf elfPairParser
