module Day3
  (day3part1, day3part2)
  where

import Common
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Instruction = Enable | Disable | Multiply (Int, Int) deriving Show

multiplyParser :: Parser (Int, Int)
multiplyParser = do
  _ <- string "mul("
  [a,b] <- decimal `sepBy` char ','
  _ <- char ')'
  return (a,b)

instructionParser :: Parser Instruction
instructionParser = choice
  [ Enable   <$  string "do()"
  , Disable  <$  string "don't()"
  , Multiply <$> multiplyParser
  ]

inputParser :: Parser [Instruction]
inputParser = many $ skipUntilP instructionParser

parseInput :: String -> [Instruction]
parseInput input = fromRight [] $ parse inputParser "Day3" input

calculate1 :: Instruction -> Int
calculate1 (Multiply (a,b)) = a * b
calculate1 _ = 0

calculate2 :: (Int, Bool) -> Instruction -> (Int, Bool)
calculate2 (total, _)     Enable            = (total,         True)
calculate2 (total, _)     Disable           = (total,         False)
calculate2 (total, True)  (Multiply (a, b)) = (total + a * b, True)
calculate2 (total, False) (Multiply _)      = (total,         False)

day3part1 :: String -> String
day3part1 = show . sum . map calculate1 . parseInput

day3part2 :: String -> String
day3part2 = show . fst . foldl calculate2 (0, True) . parseInput
