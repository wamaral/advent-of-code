{-# LANGUAGE RecordWildCards #-}

module Day2
  (day2part1, day2part2)
  where

import Common
import Text.Megaparsec
import Text.Megaparsec.Char

data Cube = Red Int | Green Int | Blue Int deriving (Show, Eq)
data Game = Game { gameId :: Int, reveals :: [[Cube]] } deriving Show

cubeParser :: Parser Cube
cubeParser = choice
  [ Red   <$> try (intParser <* string " red")
  , Green <$> try (intParser <* string " green")
  , Blue  <$> try (intParser <* string " blue")
  ]

revealParser :: Parser [Cube]
revealParser = many (cubeParser <* optional (string ", "))

gameParser :: Parser Game
gameParser = do
  _ <- string "Game "
  gameId <- intParser
  _ <- string ": "
  reveals <- manyTill (revealParser <* optional (string "; ")) newline
  pure Game{..}

cubeWithinLimit :: Cube -> Bool
cubeWithinLimit (Red n)   = n <= 12
cubeWithinLimit (Green n) = n <= 13
cubeWithinLimit (Blue n)  = n <= 14

isValidGame :: Game -> Bool
isValidGame g = all isValidReveal $ reveals g
  where isValidReveal r = all cubeWithinLimit r

day2part1 :: String -> String
day2part1 = show . sum . map gameId . filter isValidGame . readListOf gameParser

day2part2 :: String -> String
day2part2 _ = ""
