module Day5
  (day5part1, day5part2)
  where

import           Common
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Half = Upper | Lower deriving Show
data SeatCoord = SeatCoord
  { rows :: [Half]
  , cols :: [Half]
  } deriving Show

halfParser :: Parser Half
halfParser = choice
  [ Lower <$ (char 'F' <|> char 'L')
  , Upper <$ (char 'B' <|> char 'R')
  ]

seatCoordParser :: Parser SeatCoord
seatCoordParser = SeatCoord <$> count 7 halfParser <*> count 3 halfParser

search :: [Half] -> Int -> Int -> Int
search [] l _           = l
search (Lower : xs) l h = search xs l (h - (h - l + 1) `div` 2)
search (Upper : xs) l h = search xs (l + (h - l + 1) `div` 2) h

seatId :: SeatCoord -> Int
seatId coords = row * 8 + col
  where
    row = search (rows coords) 0 127
    col = search (cols coords) 0 7

day5part1 :: String -> String
day5part1 = show . maximum . map seatId . readListOf seatCoordParser

day5part2 :: String -> String
day5part2 _ = ""
