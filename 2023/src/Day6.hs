module Day6
  (day6part1, day6part2)
  where

import Common
import Data.Function
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data Race = Race { time :: Int, distance :: Int } deriving Show

inputParser :: Parser [Race]
inputParser = do
  _ <- string "Time:"
  _ <- hspace
  times <- many (intParser <* hspace)
  _ <- newline
  _ <- string "Distance:"
  _ <- hspace
  distances <- many (intParser <* hspace)
  _ <- newline
  _ <- eof
  pure $ zipWith Race times distances

parseInput :: String -> [Race]
parseInput = fromMaybe [] . parseMaybe inputParser

mergeRace :: [Race] -> Race
mergeRace rs = Race{time = concatInts (map time rs), distance = concatInts (map distance rs)}

-- I mean, I'm sure there's a formula, but this works... For now :)
-- Day2: meh, fast enough!
runRace :: Race -> Int
runRace (Race t d) = [1..t]
  & map (\n -> n * (t - n))
  & filter (> d)
  & length

day6part1 :: String -> String
day6part1 = show . product . map runRace . parseInput

day6part2 :: String -> String
day6part2 = show . runRace . mergeRace . parseInput
