module Day9
  (day9part1, day9part2)
  where

import           Common
import           Data.List
import qualified Data.Set             as S
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Command = U | D | L | R deriving Show
type Coord = V2 Int
type Rope = (Coord, Coord)

commandParser :: Parser [Command]
commandParser = do
  cmd <- choice [ U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R']
  _ <- hspace
  n <- intParser
  pure $ replicate n cmd

origin :: Rope
origin = (V2 0 0, V2 0 0)

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

move :: Rope -> Command -> Rope
move (h, t) cmd = do
  let newH = h + direction cmd
  let newT = if isTouching newH t then t else h
  (newH, newT)

day9part1 :: String -> String
day9part1 = show . S.size . S.fromList . map snd . scanl' move origin .  concat . readListOf commandParser

day9part2 :: String -> String
day9part2 _ = ""
