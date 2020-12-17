module Day17
  (day17part1, day17part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map.Strict      as M
import           Linear.V3
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Cube = Active | Inactive deriving (Eq, Show)

type Point = V3 Int
type World = M.Map Point Cube

cubeParser :: Parser Cube
cubeParser = choice
  [ Active <$ char '#'
  , Inactive <$ char '.'
  ]

emptyWorld :: Int -> Int -> Int -> World
emptyWorld x y z = foldl' (\m' p -> M.insert p Inactive m') M.empty [V3 x' y' z' | x' <- [-x..x], y' <- [-y..y], z' <- [-z..z]]

mkWorld :: Int -> [[Cube]] -> World
mkWorld growth cubes = foldl' (\m (y, cubeLine) -> foldl (\m' (x, cube) -> M.insert (V3 x y 0) cube m') m cubeLine) M.empty indexedCubes & (`M.union` world)
  where
    x' = headDef [] cubes & length
    y' = length cubes
    indexedCubes = zip [(- y' `div` 2)..] $ map (zip [(- x' `div` 2)..]) cubes
    world = emptyWorld (x' + growth * 2) (y' + growth * 2) (growth * 2)

adjacentDirections :: [V3 Int]
adjacentDirections = [V3 x y z | x <- range, y <- range, z <- range, x /= 0 || y /= 0 || z /= 0]
  where range = [-1,0,1]

adjacentCubes :: Point -> World -> [Cube]
adjacentCubes point world = map (lookupDef . (+ point)) adjacentDirections
  where lookupDef p = M.findWithDefault Inactive p world

numActiveCubes :: [Cube] -> Int
numActiveCubes = length . filter (== Active)

updateCube :: Cube -> [Cube] -> Cube
updateCube Inactive neighbours = if numActiveCubes neighbours == 3 then Active else Inactive
updateCube Active neighbours = if numActive == 2 || numActive == 3 then Active else Inactive
  where numActive = numActiveCubes neighbours

nextIteration :: World -> World
nextIteration world = M.mapWithKey (\point cube -> updateCube cube (adjacentCubes point world)) world

day17part1 :: String -> String
day17part1 = show . length . filter (== Active) . M.elems . last . take (succ runs) . iterate nextIteration . mkWorld runs . readListOf (many cubeParser <* newline)
  where runs = 6

day17part2 :: String -> String
day17part2 _ = ""
