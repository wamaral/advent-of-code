module Day1 where

import Data.List (uncons)
import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)

-- Types
data Orientation = N | E | S | W deriving (Show, Eq, Ord)
data Movement = R Integer | L Integer deriving Show
data Position = Position { x :: Integer
                         , y :: Integer
                         , orientation :: Orientation
                         } deriving (Show, Ord)
instance Eq Position where
  (==) p1 p2 = (x p1) == (x p2) && (y p1) == (y p2)

-- Type functions
initPos :: Position
initPos = Position { x = 0 , y = 0 , orientation = N }

walkDistance :: Movement -> Integer
walkDistance (R a) = a
walkDistance (L a) = a

-- Walk
spinAround :: Orientation -> Movement -> Orientation
spinAround current mov = head . tail $ dropWhile (/= current) (op orientations)
  where
    orientations = [N, E, S, W, N]
    op = case mov of
      R _ -> id
      L _ -> reverse

turn :: Position -> Movement -> Position
turn current mov = current { orientation = spinAround (orientation current) mov}

walk :: Position -> Movement -> Position
walk current mov = case (orientation current) of
  N -> current { y = (y current) + dist}
  S -> current { y = (y current) - dist}
  E -> current { x = (x current) + dist}
  W -> current { x = (x current) - dist}
  where dist = walkDistance mov

distance :: Position -> Integer
distance pos = abs (x pos) + abs (y pos)

repeated :: [Position] -> [Position]
repeated [] = []
repeated (x:xs) = if x `elem` xs then x:(repeated xs) else (repeated xs)

-- Parse
moveMaker :: (Char, String) -> Movement
moveMaker pair = case pair of
  ('R', xs) -> R (parse xs)
  ('L', xs) -> L (parse xs)
  _ -> error "Invalid input"
  where
    parse x = read x :: Integer

-- Main
main :: IO ()
main = do
  c <- readFile "input/1"
  let steps = map moveMaker $ mapMaybe uncons $ splitOneOf ", " c
  let go = (\pos mov -> walk (turn pos mov) mov)
  let path = scanl go initPos steps
  let final = last path
  putStr "1. "
  putStrLn $ show $ distance final
  putStr "2. "
  putStrLn $ show $ distance $ head $ repeated path
