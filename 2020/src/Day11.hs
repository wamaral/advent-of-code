module Day11
  (day11part1, day11part2)
  where

import           Common
import           Data.Function
import qualified Data.Map             as M
import           Data.Maybe
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Seat = Floor | Empty | Occupied deriving (Eq, Show)
type Coord = V2 Int
type Grid = M.Map Coord Seat

seatParser :: Parser Seat
seatParser = choice
  [ Floor <$ char '.'
  , Empty <$ char 'L'
  , Occupied <$ char '#'
  ]

inputToGrid :: [[Seat]] -> Grid
inputToGrid xs = foldl (\m (y, seats) -> foldl (\m' (x, seat) -> M.insert (V2 x y) seat m') m seats) M.empty $ zip [0..] $ map (zip [0..]) xs -- ouch

adjacentIndexes :: Coord -> [Coord]
adjacentIndexes v = map (v +) adjList
  where adjList = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                   V2 (-1) 0,               V2 1 0,
                   V2 (-1) 1,    V2 0 1,    V2 1 1]

adjacents :: Grid -> Coord -> [Seat]
adjacents grid coord = mapMaybe (`M.lookup` grid) $ adjacentIndexes coord

changeSeat :: Seat -> [Seat] -> Seat
changeSeat Empty adj    = if Occupied `notElem` adj then Occupied else Empty
changeSeat Occupied adj = if filter (== Occupied) adj & length & (>= 4) then Empty else Occupied
changeSeat seat _       = seat

changeGrid :: Grid -> Grid
changeGrid grid = M.mapWithKey f grid
  where f coord seat = changeSeat seat (adjacents grid coord)

fixpoint :: Grid -> Grid
fixpoint grid = if grid == newGrid then grid else fixpoint newGrid
  where newGrid = changeGrid grid

day11part1 :: String -> String
day11part1 = show . M.size . M.filter (== Occupied) . fixpoint . inputToGrid . readListOf (many seatParser)

day11part2 :: String -> String
day11part2 _ = ""
