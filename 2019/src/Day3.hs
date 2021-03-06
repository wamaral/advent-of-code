module Day3
  (day3part1, day3part2)
  where

import           Common
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Point = Point Int Int deriving (Eq, Ord, Show)

data Direction = U | D | L | R deriving Show

data Movement = Movement Direction Int deriving Show

directionParser :: Parser Direction
directionParser = choice
  [ U <$ char 'U'
  , D <$ char 'D'
  , L <$ char 'L'
  , R <$ char 'R'
  ]

movementParser :: Parser Movement
movementParser = Movement <$> directionParser <*> intParser

readMovements :: String -> [Movement]
readMovements = fromMaybe [] . parseMaybe (movementParser `sepEndBy` char ',')

manhattanDistance :: Point -> Int
manhattanDistance  (Point x y) = abs x + abs y

move :: Point -> Movement -> [Point]
move (Point x y) (Movement U n) = [Point x y' | y' <- [y..(y+n)]]
move (Point x y) (Movement D n) = [Point x y' | y' <- reverse [(y-n)..y]]
move (Point x y) (Movement L n) = [Point x' y | x' <- reverse [(x-n)..x]]
move (Point x y) (Movement R n) = [Point x' y | x' <- [x..(x+n)]]

path :: [Movement] -> [Point]
path = foldl moveFromLast [Point 0 0]
  where moveFromLast points movement = points ++ tail (move (last points) movement)

paths :: String -> ([Point], [Point])
paths input = (head ps, head $ tail ps)
  where ps = map (tail . path . readMovements) $ lines input

day3part1 :: String -> String
day3part1 input = show $ minimum $ S.map manhattanDistance $ S.intersection spath1 spath2
  where (spath1, spath2) = bimap S.fromList S.fromList $ paths input

day3part2 :: String -> String
day3part2 input = show $ minimum lengths
  where
    (path1, path2) = paths input
    intersections = S.toList $ S.intersection (S.fromList path1) (S.fromList path2)
    lenPathUntil point path' = length (takeWhile (/= point) path') + 1 -- include end point
    lengths = map (\i -> lenPathUntil i path1 + lenPathUntil i path2) intersections
