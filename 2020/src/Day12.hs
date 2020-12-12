module Day12
  (day12part1, day12part2)
  where

import           Common
import           Data.Function
import           Linear.V2
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
newShip = Ship { orientation = East, position = V2 0 0 }

translate :: V2 Int -> Orientation -> Int -> V2 Int
translate pos North n = pos + V2 0 n
translate pos South n = pos + V2 0 (- n)
translate pos East n  = pos + V2 n 0
translate pos West n  = pos + V2 (- n) 0

rotate :: Orientation -> Int -> Orientation
rotate o n = lastDef o $ map snd rotationList
  where
    clockwise = [North, East, South, West]
    counterclockwise = reverse clockwise
    rotationList = (if n > 0 then clockwise else counterclockwise) & cycle & dropWhile (/= o) & zip [0,90..(abs n)]

moveShip :: Ship -> Instruction -> Ship
moveShip ship (MoveNorth n) = ship {position = translate (position ship) North n}
moveShip ship (MoveSouth n) = ship {position = translate (position ship) South n}
moveShip ship (MoveEast n) = ship {position = translate (position ship) East n}
moveShip ship (MoveWest n) = ship {position = translate (position ship) West n}
moveShip ship (TurnLeft n)    = ship {orientation = rotate (orientation ship) (- n)}
moveShip ship (TurnRight n)    = ship {orientation = rotate (orientation ship) n}
moveShip ship (MoveForward n) = ship {position = translate (position ship) (orientation ship) n}

day12part1 :: String -> String
day12part1 = show . sum . abs . position . foldl moveShip newShip . readListOf instructionParser

day12part2 :: String -> String
day12part2 _ = ""
