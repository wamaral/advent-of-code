module Day24
  (day24part1, day24part2)
  where

import           Common
import           Data.List
import           Linear.V3
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Direction = East | West | Southeast | Southwest | Northeast | Northwest deriving Show
type Tilepath = [Direction]
type Coord = V3 Int

directionParser :: Parser Direction
directionParser = choice
  [ East <$ char 'e'
  , West <$ char 'w'
  , Southeast <$ try (string "se")
  , Southwest <$ try (string "sw")
  , Northeast <$ try (string "ne")
  , Northwest <$ try (string "nw")
  ]

tilepathParser :: Parser Tilepath
tilepathParser = many directionParser

-- | Cube coordinates from https://www.redblobgames.com/grids/hexagons/
directionToCoord :: Direction -> Coord
directionToCoord East      = V3 1    (-1) 0
directionToCoord West      = V3 (-1) 1    0
directionToCoord Southeast = V3 0    (-1) 1
directionToCoord Southwest = V3 (-1) 0    1
directionToCoord Northeast = V3 1    0    (-1)
directionToCoord Northwest = V3 0    1    (-1)

walk :: Coord -> Direction -> Coord
walk tile direction = tile + directionToCoord direction

day24part1 :: String -> String
day24part1 = show . length . filter (odd . length) . group . sort . map (foldl' walk (V3 0 0 0)) . readListOf tilepathParser

day24part2 :: String -> String
day24part2 _ = ""
