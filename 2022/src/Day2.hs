module Day2
  (day2part1, day2part2)
  where

import           Common
import           Text.Megaparsec
import           Text.Megaparsec.Char

data RPS = Rock | Paper | Scissor deriving (Eq, Show)

rpsParser :: Parser RPS
rpsParser = choice
  [ Rock <$ (char 'A' <|> char 'X')
  , Paper <$ (char 'B' <|> char 'Y')
  , Scissor <$ (char 'C' <|> char 'Z')
  ]

lineParser :: Parser (RPS, RPS)
lineParser = (,) <$> (rpsParser <* hspace) <*> rpsParser

shapeScore :: RPS -> Int
shapeScore Rock    = 1
shapeScore Paper   = 2
shapeScore Scissor = 3

outcomeScore :: (RPS, RPS) -> Int
outcomeScore (Scissor, Rock)  = 6
outcomeScore (Paper, Scissor) = 6
outcomeScore (Rock, Paper)    = 6
outcomeScore (a, b)           = if a == b then 3 else 0

roundScore :: (RPS, RPS) -> Int
roundScore l@(_, b) = outcomeScore l + shapeScore b

day2part1 :: String -> String
day2part1 = show . sum . map roundScore . readListOf lineParser

day2part2 :: String -> String
day2part2 _ = ""
