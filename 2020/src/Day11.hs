module Day11
  (day11part1, day11part2)
  where

import           Common
import           Data.Function
import qualified Data.Map             as M
import           Data.Maybe
import           Linear.V2
import           Safe
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

adjacentDirections :: [V2 Int]
adjacentDirections = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                      V2 (-1) 0,               V2 1 0,
                      V2 (-1) 1,    V2 0 1,    V2 1 1]

kingAdjacentIndexes :: Coord -> [Coord]
kingAdjacentIndexes coord = map (+ coord) adjacentDirections

kingAdjacents :: Grid -> Coord -> [Seat]
kingAdjacents grid coord = mapMaybe (`M.lookup` grid) $ kingAdjacentIndexes coord

infiniteQueenAdjacentIndexes :: Coord -> [[Coord]]
infiniteQueenAdjacentIndexes coord = map (\x -> tail $ iterate (+ x) coord) adjacentDirections

queenAdjacents :: Grid -> Coord -> [Seat]
queenAdjacents grid coord = map whatCanISee $ infiniteQueenAdjacentIndexes coord
  where whatCanISee coords = map (`M.lookup` grid) coords & takeWhile isJust & catMaybes & filter (/= Floor) & headDef Floor

changeSeat :: Seat -> [Seat] -> Int -> Seat
changeSeat Empty adj _             = if Occupied `notElem` adj then Occupied else Empty
changeSeat Occupied adj neighbours = if filter (== Occupied) adj & length & (>= neighbours) then Empty else Occupied
changeSeat seat _ _                = seat

changeGrid :: (Grid -> Coord -> [Seat]) -> Int -> Grid -> Grid
changeGrid f x grid = M.mapWithKey f' grid
  where f' coord seat = changeSeat seat (f grid coord) x

changeGridKing :: Grid -> Grid
changeGridKing = changeGrid kingAdjacents 4

changeGridQueen :: Grid -> Grid
changeGridQueen = changeGrid queenAdjacents 5

fixpoint :: (Grid -> Grid) -> Grid -> Grid
fixpoint f grid = if grid == newGrid then grid else fixpoint f newGrid
  where newGrid = f grid

day11part1 :: String -> String
day11part1 = show . M.size . M.filter (== Occupied) . fixpoint changeGridKing . inputToGrid . readListOf (many seatParser)

day11part2 :: String -> String
day11part2 = show . M.size . M.filter (== Occupied) . fixpoint changeGridQueen . inputToGrid . readListOf (many seatParser)
