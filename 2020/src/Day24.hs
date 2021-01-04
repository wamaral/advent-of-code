module Day24
  (day24part1, day24part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Set             as S
import           Linear.V3
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Direction = East | West | Southeast | Southwest | Northeast | Northwest deriving (Enum, Show)
type Tilepath = [Direction]
type Coord = V3 Int
type Grid = S.Set Coord

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

neighbourCoords :: Coord -> [Coord]
neighbourCoords coord = map (walk coord) [East .. Northwest]

activeNeighbours :: Coord -> Grid -> Grid
activeNeighbours coord grid = S.fromList $ filter (`S.member` grid) (neighbourCoords coord)

gridToCheck :: Grid -> Grid
gridToCheck grid = S.toList grid
  & concatMap neighbourCoords
  & S.fromList
  & S.union grid

flipGrid :: Grid -> Grid
flipGrid grid = gridToCheck grid
  & S.filter flipTile
  where
    countNeighbours tile = activeNeighbours tile grid & S.size
    flipTile tile = if S.member tile grid
      then countNeighbours tile == 1 || countNeighbours tile == 2
      else countNeighbours tile == 2

initialState :: [Tilepath] -> Grid
initialState paths = paths
  & map (foldl' walk (V3 0 0 0))
  & sort
  & group
  & filter (odd . length)
  & map head
  & S.fromList

day24part1 :: String -> String
day24part1 input = readListOf tilepathParser input
  & initialState
  & S.size
  & show

day24part2 :: String -> String
day24part2 input = readListOf tilepathParser input
  & initialState
  & iterate flipGrid
  & take 101
  & lastDef S.empty
  & S.size
  & show
