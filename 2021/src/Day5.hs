module Day5
  (day5part1, day5part2)
  where

import           Common
import           Control.Lens
import qualified Data.Map             as M
import           Linear.V2
import           Linear.Vector
import           Text.Megaparsec.Char

type Point = V2 Int
type Line = [Point]

parsePoints :: Parser (Point, Point)
parsePoints = do
  x1 <- intParser
  _ <- char ','
  y1 <- intParser
  _ <- string " -> "
  x2 <- intParser
  _ <- char ','
  y2 <- intParser
  return (V2 x1 y1, V2 x2 y2)

makeLine :: (Point, Point) -> Line
makeLine (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = map (V2 x1) (interpolate y1 y2)
  | y1 == y2 = map (`V2` y1) (interpolate x1 x2)
  | otherwise = zipWith V2 (interpolate x1 x2) (interpolate y1 y2)

isStraight :: Line -> Bool
isStraight ps = 0 `elem` [diff ^._x, diff ^._y]
  where diff = head ps ^-^ last ps

day5part1 :: String -> String
day5part1 = show
  . M.size
  . M.filter (> 1)
  . M.fromListWith (+)
  . flip zip (repeat 1 :: [Int])
  . concat
  . filter isStraight
  . map makeLine
  . readListOf parsePoints

day5part2 :: String -> String
day5part2 = show
  . M.size
  . M.filter (> 1)
  . M.fromListWith (+)
  . flip zip (repeat 1 :: [Int])
  . concatMap makeLine
  . readListOf parsePoints
