module Day2
  (day2part1, day2part2)
  where

import           Common
import           Control.Lens         ((^.))
import           Linear.V2
import           Linear.V3
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

commandVector :: Command -> V2 Int
commandVector (Forward n) = V2 n 0
commandVector (Down n)    = V2 0 n
commandVector (Up n)      = V2 0 (negate n)

commandMove :: V3 Int -> Command -> V3 Int
commandMove (V3 x y z) (Forward n) = V3 (x + n) (y + n * z) z
commandMove pos (Down n)           = pos + V3 0 0 n
commandMove pos (Up n)             = pos + V3 0 0 (negate n)

multCoords :: V2 Int -> Int
multCoords (V2 x y) = x * y

day2part1 :: String -> String
day2part1 = show . multCoords . sumV . map commandVector . readListOf commandParser

day2part2 :: String -> String
day2part2 = show . multCoords . (^. _xy) . foldl commandMove (V3 0 0 0) . readListOf commandParser
