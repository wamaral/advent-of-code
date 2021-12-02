module Day2
  (day2part1, day2part2)
  where

import           Common
import           Linear.V2
import           Linear.Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Command = Forward Int | Down Int | Up Int deriving Show

commandParser :: Parser Command
commandParser = choice
  [ Forward <$> (string "forward " *> intParser)
  , Down <$> (string "down " *> intParser)
  , Up <$> (string "up " *> intParser)
  ]

commandMovement :: Command -> V2 Int
commandMovement (Forward n) = V2 n 0
commandMovement (Down n)    = V2 0 n
commandMovement (Up n)      = V2 0 (negate n)

multCoords :: V2 Int -> Int
multCoords (V2 x y) = x * y

day2part1 :: String -> String
day2part1 = show . multCoords . sumV . map commandMovement . readListOf commandParser

day2part2 :: String -> String
day2part2 _ = ""
