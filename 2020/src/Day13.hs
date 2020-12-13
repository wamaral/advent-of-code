{-# LANGUAGE TupleSections #-}

module Day13
  (day13part1, day13part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type MyDepart = Int
type Bus = Int

busParser :: Parser (Maybe Bus)
busParser = choice
  [ Just <$> intParser
  , Nothing <$ char 'x'
  ]

inputParser :: Parser (MyDepart, [Maybe Bus])
inputParser = do
  myDepart <- intParser
  _ <- newline
  busIds <- busParser `sepBy` char ','
  _ <- optional newline
  _ <- eof
  return (myDepart, busIds)

parseInput :: String -> (MyDepart, [Maybe Bus])
parseInput = fromMaybe (0, []) . parseMaybe inputParser

busSchedule :: Bus -> [Int]
busSchedule b = [0,b..]

nextScheduleAfterMyDepart :: MyDepart -> Bus -> (Bus, Int)
nextScheduleAfterMyDepart myDepart bus = (bus,) <$> headDef 0 $ dropWhile (< myDepart) (busSchedule bus)

day13part1 :: String -> String
day13part1 input = catMaybes busses
  & map (nextScheduleAfterMyDepart myDepart)
  & minimumBy (comparing snd)
  & (\(bus, depart) -> bus * (depart - myDepart))
  & show
  where (myDepart, busses) = parseInput input

day13part2 :: String -> String
day13part2 _ = ""
