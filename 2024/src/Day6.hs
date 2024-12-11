{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Day6
  (day6part1, day6part2)
  where

import           Common
import           Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Position = V2 Int
data Direction = U | R | D | L deriving (Show, Enum, Eq, Ord)
data Guard = Guard { facing :: Direction, position :: Position, active :: Bool, looping :: Bool, beenThere :: BeenThere } deriving Show
data Location = Start | Empty | Blocked deriving (Show, Eq)
type Visited = S.Set Position
type BeenThere = S.Set (Position, Direction)
type Grid = M.Map Position Location
data SimulationState = SimulationState { grid :: Grid, guard :: Guard, visited :: Visited } deriving Show

locationParser :: Parser Location
locationParser = choice
  [ Start <$ char '^'
  , Empty <$ char '.'
  , Blocked <$ char '#'
  ]

parseGrid :: String -> Grid
parseGrid = listOfListsToV2Map . readListOf (some locationParser)

initGuard :: Grid -> Guard
initGuard grid = Guard { active = True, looping = False, facing = U, position = head $ M.keys $ M.filter (== Start) grid, beenThere = S.empty }

turnRight :: Direction -> Direction
turnRight L = U
turnRight d = succ d

los :: Direction -> Position -> [Position]
los U (V2 x y) = map (V2 x) (reverse [0 .. y])
los D (V2 x y) = map (V2 x) [y..]
los R (V2 x y) = map (`V2` y) [x..]
los L (V2 x y) = map (`V2` y) (reverse [0 .. x])

isBorder :: Grid -> Position -> Bool
isBorder grid (V2 x y) = x == 0 || y == 0 || M.notMember (V2 (succ x) y) grid || M.notMember (V2 x (succ y)) grid

isLoop :: Guard -> Position -> Direction -> Bool
isLoop guard p d = S.member (p, d) (beenThere guard)

pathToNextBlock :: Grid -> Guard -> [Position]
pathToNextBlock grid guard = los (facing guard) (position guard) & takeWhile (`M.member` grid) & takeWhile (\p -> M.findWithDefault Blocked p grid /= Blocked)

walk :: SimulationState -> SimulationState
walk (SimulationState grid guard visited) = SimulationState { grid = grid, guard = newGuard, visited = S.union visited (S.fromList path) }
  where
    path = pathToNextBlock grid guard
    leaving = isBorder grid (last path)
    looping = isLoop guard (last path) newFacing
    newFacing = turnRight (facing guard)
    newGuard = guard {active = not leaving, looping = looping, facing = newFacing, position = last path, beenThere = S.insert (last path, newFacing) guard.beenThere}

day6part1 :: String -> String
day6part1 input = initState
  & iterate walk
  & dropWhile (\s -> s.guard.active)
  & head
  & (.visited)
  & S.size
  & show
  where
    grid = parseGrid input
    guard = initGuard grid
    visited = S.empty
    initState = SimulationState{..}

-- Slooooooooooooow, needs refactoring :)
day6part2 :: String -> String
day6part2 input = allInitStates
  & map runState
  & filter (\s -> s.guard.looping)
  & length
  & show
  where
    grid = parseGrid input
    guard = initGuard grid
    allGrids = map (\k -> M.insert k Blocked grid) (M.keys $ M.filter (== Empty) grid)
    allInitStates = map (\g -> SimulationState{ grid = g, guard = guard, visited = S.empty }) allGrids
    runState state = state & iterate walk & dropWhile (\s -> s.guard.active && not s.guard.looping) & head
