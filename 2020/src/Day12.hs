module Day12
  (day12part1, day12part2)
  where

import           Common
import           Data.Function
import           Linear.V2
import           Linear.Vector        ((^*))
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Orientation = North | South | East | West deriving (Eq, Show)

data Instruction = MoveNorth Int
  | MoveSouth Int
  | MoveEast Int
  | MoveWest Int
  | TurnLeft Int
  | TurnRight Int
  | MoveForward Int
  deriving Show

data Ship = Ship
  { orientation :: Orientation
  , position    :: V2 Int
  , waypoint    :: V2 Int
  } deriving Show

instructionParser :: Parser Instruction
instructionParser = choice
  [ MoveNorth <$> (char 'N' *> intParser)
  , MoveSouth <$> (char 'S' *> intParser)
  , MoveEast <$> (char 'E' *> intParser)
  , MoveWest <$> (char 'W' *> intParser)
  , TurnLeft <$> (char 'L' *> intParser)
  , TurnRight <$> (char 'R' *> intParser)
  , MoveForward <$> (char 'F' *> intParser)
  ]

newShip :: Ship
newShip = Ship { orientation = East, position = V2 0 0, waypoint = V2 10 1 }

translate :: V2 Int -> Orientation -> Int -> V2 Int
translate pos North n = pos + V2 0 n
translate pos South n = pos + V2 0 (- n)
translate pos East n  = pos + V2 n 0
translate pos West n  = pos + V2 (- n) 0

rotateShip :: Orientation -> Int -> Orientation
rotateShip o n = lastDef o $ map snd rotationList
  where
    clockwise = [North, East, South, West]
    counterclockwise = reverse clockwise
    rotationList = (if n > 0 then clockwise else counterclockwise) & cycle & dropWhile (/= o) & zip [0,90..(abs n)]

rotateWaypoint :: V2 Int -> Int -> V2 Int
rotateWaypoint w n = lastDef w $ take (abs n `div` 90) $ tail $ iterate rotationFn w
  where
    rotateCcw = perp
    rotateCw (V2 x y) = V2 y (negate x) -- Why does the linear library only support counter-clockwise??
    rotationFn = if n > 0 then rotateCw else rotateCcw

navigate :: Ship -> Instruction -> Ship
navigate ship (MoveNorth n)   = ship {position = translate (position ship) North n}
navigate ship (MoveSouth n)   = ship {position = translate (position ship) South n}
navigate ship (MoveEast n)    = ship {position = translate (position ship) East n}
navigate ship (MoveWest n)    = ship {position = translate (position ship) West n}
navigate ship (TurnLeft n)    = ship {orientation = rotateShip (orientation ship) (- n)}
navigate ship (TurnRight n)   = ship {orientation = rotateShip (orientation ship) n}
navigate ship (MoveForward n) = ship {position = translate (position ship) (orientation ship) n}

navigateToWaypoint :: Ship -> Instruction -> Ship
navigateToWaypoint ship (MoveNorth n)   = ship {waypoint = translate (waypoint ship) North n}
navigateToWaypoint ship (MoveSouth n)   = ship {waypoint = translate (waypoint ship) South n}
navigateToWaypoint ship (MoveEast n)    = ship {waypoint = translate (waypoint ship) East n}
navigateToWaypoint ship (MoveWest n)    = ship {waypoint = translate (waypoint ship) West n}
navigateToWaypoint ship (TurnLeft n)    = ship {waypoint = rotateWaypoint (waypoint ship) (- n)}
navigateToWaypoint ship (TurnRight n)   = ship {waypoint = rotateWaypoint (waypoint ship) n}
navigateToWaypoint ship (MoveForward n) = ship {position = position ship + (waypoint ship ^* n)}

day12part1 :: String -> String
day12part1 = show . sum . abs . position . foldl navigate newShip . readListOf instructionParser

day12part2 :: String -> String
day12part2 = show . sum . abs . position . foldl navigateToWaypoint newShip . readListOf instructionParser
