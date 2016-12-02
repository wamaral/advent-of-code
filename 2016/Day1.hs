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
  (==) p p' = (x p) == (x p') && (y p) == (y p')

-- Type functions
initPos :: Position
initPos = Position { x = 0, y = 0, orientation = N }

walkDistance :: Movement -> Integer
walkDistance (R a) = a
walkDistance (L a) = a

-- Walk

walk :: [Position] -> Movement -> [Position]
walk path mov = path ++ map (fn $ last path) [1..(walkDistance mov)]
  where fn p = case orientation p of
          N -> (\i -> p { y = (y p) + i })
          S -> (\i -> p { y = (y p) - i })
          E -> (\i -> p { x = (x p) + i })
          W -> (\i -> p { x = (x p) - i })

spinAround :: Orientation -> Movement -> Orientation
spinAround current mov = head . tail $ dropWhile (/= current) (op orientations)
  where
    orientations = [N, E, S, W, N]
    op = case mov of
      R _ -> id
      L _ -> reverse

turn :: [Position] -> Movement -> [Position]
turn path mov = (init path) ++ [updated]
  where updated = (last path) { orientation = spinAround (orientation (last path)) mov}

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
  let go = (\path mov -> walk (turn path mov) mov)
  let path = foldl go [initPos] steps
  let final = last path
  putStr "1. "
  putStrLn $ show $ distance final
  putStr "2. "
  putStrLn $ show $ distance $ head $ repeated path
