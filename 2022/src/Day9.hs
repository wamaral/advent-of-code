module Day9
  (day9part1, day9part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Set             as S
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Command = U | D | L | R deriving Show
type Coord = V2 Int
type Rope = [Coord]

commandParser :: Parser [Command]
commandParser = do
  cmd <- choice [ U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R']
  _ <- hspace
  n <- intParser
  pure $ replicate n cmd

origin :: Int -> Rope
origin len = replicate len (V2 0 0)

direction :: Command -> Coord
direction U = V2 0 (-1)
direction D = V2 0 1
direction L = V2 (-1) 0
direction R = V2 1 0

neighbourhood :: [Coord]
neighbourhood = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                 V2 (-1) 0,    V2 0 0,    V2 1 0,
                 V2 (-1) 1,    V2 0 1,    V2 1 1]

isTouching :: Coord -> Coord -> Bool
isTouching a b = any ((== b) . (+ a)) neighbourhood

shrink :: Int -> Int
shrink n
  | n > 1 = 1
  | n < (-1) = -1
  | otherwise = n

moveHead :: Coord -> Command -> Coord
moveHead h cmd = h + direction cmd

moveSegment :: Coord -> Coord -> Coord
moveSegment h t = if isTouching h t then t else t + diagonal h t
  where diagonal a b = a - b & (\(V2 x y) -> V2 (shrink x) (shrink y))

move :: Rope -> Command -> Rope
move ropes cmd = foldl' moveTail [moveHead (head ropes) cmd] (tail ropes)
  where moveTail newRopes segment = newRopes ++ [moveSegment (last newRopes) segment]

solveForLength :: Int -> [Command] -> Int
solveForLength n = S.size . S.fromList . map last . scanl' move (origin n)

day9part1 :: String -> String
day9part1 = show . solveForLength 2 . concat . readListOf commandParser

day9part2 :: String -> String
day9part2 = show . solveForLength 10 . concat . readListOf commandParser
