{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Day2
  (day2part1, day2part2)
  where

import Common
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data CubeColor = R | G | B deriving (Show, Eq)
type Cube = (CubeColor, Int)
data Reveal = Reveal { r :: Int, g :: Int, b :: Int } deriving (Show, Eq)
data Game = Game { gameId :: Int, reveals :: [Reveal] } deriving Show

cubeParser :: Parser Cube
cubeParser = choice
  [ (R,) <$> try (intParser <* string " red")
  , (G,) <$> try (intParser <* string " green")
  , (B,) <$> try (intParser <* string " blue")
  ]

revealParser :: Parser Reveal
revealParser = makeReveal <$> many (cubeParser <* optional (string ", "))
  where
    getCube :: CubeColor -> [Cube] -> Int
    getCube _ [] = 0
    getCube c cs = fromMaybe 0 $ lookup c cs
    makeReveal :: [Cube] -> Reveal
    makeReveal cs = Reveal
      { r = getCube R cs
      , g = getCube G cs
      , b = getCube B cs
      }

gameParser :: Parser Game
gameParser = do
  _ <- string "Game "
  gameId <- intParser
  _ <- string ": "
  reveals <- manyTill (revealParser <* optional (string "; ")) newline
  pure Game{..}

isValidReveal :: Reveal -> Bool
isValidReveal reveal = r reveal <= 12 && g reveal <= 13 && b reveal <= 14

isValidGame :: Game -> Bool
isValidGame = all isValidReveal . reveals

maxReveals :: Reveal -> Reveal -> Reveal
maxReveals r1 r2 = Reveal
  { r = max (r r1) (r r2)
  , g = max (g r1) (g r2)
  , b = max (b r1) (b r2)
  }

minGame :: Game -> Reveal
minGame game = foldl1 maxReveals (reveals game)

powerReveal :: Reveal -> Int
powerReveal reveal = r reveal * g reveal * b reveal

day2part1 :: String -> String
day2part1 = show . sum . map gameId . filter isValidGame . readListOf gameParser

day2part2 :: String -> String
day2part2 = show . sum . map (powerReveal . minGame) . readListOf gameParser
