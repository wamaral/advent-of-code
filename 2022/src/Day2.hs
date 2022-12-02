module Day2
  (day2part1, day2part2)
  where

import           Common
import           Data.Bifunctor
import           Text.Megaparsec
import           Text.Megaparsec.Char

data RPS = Rock | Paper | Scissor deriving (Eq, Show)
data DesiredOutcome = Win | Draw | Lose deriving (Eq, Show)

rpsParser :: Parser RPS
rpsParser = choice
  [ Rock <$ (char 'A' <|> char 'X')
  , Paper <$ (char 'B' <|> char 'Y')
  , Scissor <$ (char 'C' <|> char 'Z')
  ]

lineParser :: Parser (RPS, RPS)
lineParser = (,) <$> (rpsParser <* hspace) <*> rpsParser

-- ¯\_(ツ)_/¯
rpsAsOutcome :: RPS -> DesiredOutcome
rpsAsOutcome Rock    = Lose
rpsAsOutcome Paper   = Draw
rpsAsOutcome Scissor = Win

losesTo :: RPS -> RPS
losesTo Rock    = Scissor
losesTo Paper   = Rock
losesTo Scissor = Paper

winsOver :: RPS -> RPS
winsOver Rock    = Paper
winsOver Paper   = Scissor
winsOver Scissor = Rock

shapeScore :: RPS -> Int
shapeScore Rock    = 1
shapeScore Paper   = 2
shapeScore Scissor = 3

outcomeScore :: (RPS, RPS) -> Int
outcomeScore (a, b)
  | winsOver a == b = 6
  | a == b          = 3
  | otherwise       = 0

roundScore :: (RPS, RPS) -> Int
roundScore l@(_, b) = outcomeScore l + shapeScore b

fixOutcome :: (RPS, DesiredOutcome) -> (RPS, RPS)
fixOutcome (a, Win)  = (a, winsOver a)
fixOutcome (a, Draw) = (a, a)
fixOutcome (a, Lose) = (a, losesTo a)

day2part1 :: String -> String
day2part1 = show . sum . map roundScore . readListOf lineParser

day2part2 :: String -> String
day2part2 = show . sum . map (roundScore . fixOutcome . second rpsAsOutcome) . readListOf lineParser
