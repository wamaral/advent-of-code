module Day17
  (day17part1, day17part2)
  where

import           Common
import           Data.Function
import qualified Data.Set             as S
import           Linear.V4            hiding (point)
import qualified Linear.Vector        as Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Cube = Active | Inactive deriving (Eq, Show)

type Point = V4 Int
data World = World3d (S.Set Point) | World4d (S.Set Point)

cubeParser :: Parser Cube
cubeParser = choice
  [ Active <$ char '#'
  , Inactive <$ char '.'
  ]

inputToPoints :: [[Cube]] -> S.Set Point
inputToPoints cubes = cubes
  & map (zip [0..])
  & zip [0..]
  & concatMap (\(y, cubeLine) -> map (\(x, cube) -> (x,y,cube)) cubeLine)
  & filter (\(_,_,c) -> c == Active)
  & map (\(x,y,_) -> V4 x y 0 0)
  & S.fromList

-- Interdimensional helpers
getFromWorld :: World -> S.Set Point
getFromWorld (World3d x) = x
getFromWorld (World4d x) = x

backToWorld :: World -> S.Set Point -> World
backToWorld (World3d _) x = World3d x
backToWorld (World4d _) x = World4d x

to3d :: Point -> Point
to3d (V4 x y z _) = V4 x y z 0

project :: World -> Point -> Point
project (World3d _) = to3d
project (World4d _) = id
-- Interdimensional helpers

setupWorld :: World -> S.Set Point -> World
setupWorld world points = S.map (project world) points & backToWorld world

worldToCheck :: World -> World
worldToCheck world = getFromWorld world & S.toList & concatMap (\p -> map (+ p) $ S.toList $ adjacentDirections world) & S.fromList & backToWorld world

adjacentDirections :: World -> S.Set Point
adjacentDirections world = [V4 x y z w | x <- range, y <- range, z <- range, w <- range]
  & map (project world)
  & S.fromList
  & S.delete Vector.zero
  where range = [-1,0,1]

nextIteration :: World -> World
nextIteration world = S.foldl' (\s point -> if shouldBeActive point world then S.insert point s else s) S.empty checkWorld & backToWorld world
  where checkWorld = worldToCheck world & getFromWorld

shouldBeActive :: Point -> World -> Bool
shouldBeActive point world = if pointInWorld then neighbourCount == 2 || neighbourCount == 3 else neighbourCount == 3
  where
    worldPoints = getFromWorld world
    pointInWorld = S.member point worldPoints
    neighbourCount = adjacentDirections world & S.map (+ point) & S.intersection worldPoints & S.size

runForWorld :: World -> String -> String
runForWorld world = show . S.size . getFromWorld . last . take (succ runs) . iterate nextIteration . setupWorld world . inputToPoints. readListOf (many cubeParser <* newline)
  where runs = 6

day17part1 :: String -> String
day17part1 = runForWorld (World3d S.empty)

day17part2 :: String -> String
day17part2 = runForWorld (World4d S.empty)
