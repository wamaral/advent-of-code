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

type MyDepart = Integer
type Bus = Integer

busParser :: Parser (Maybe Bus)
busParser = choice
  [ Just . toInteger <$> intParser
  , Nothing <$ char 'x'
  ]

inputParser :: Parser (MyDepart, [Maybe Bus])
inputParser = do
  myDepart <- toInteger <$> intParser
  _ <- newline
  busIds <- busParser `sepBy` char ','
  _ <- optional newline
  _ <- eof
  return (myDepart, busIds)

parseInput :: String -> (MyDepart, [Maybe Bus])
parseInput = fromMaybe (0, []) . parseMaybe inputParser

busSchedule :: Bus -> [Integer]
busSchedule b = [0,b..]

nextScheduleAfterMyDepart :: MyDepart -> Bus -> (Bus, Integer)
nextScheduleAfterMyDepart myDepart bus = (bus,) <$> headDef 0 $ dropWhile (< myDepart) (busSchedule bus)

indexBusses :: [Maybe Bus] -> [(Integer, Bus)]
indexBusses bs = zip [0..] bs & mapMaybe flipTheBus
  where
    flipTheBus (_, Nothing) = Nothing
    flipTheBus (x, Just b)  = Just (x,b)

matchingSchedule :: (Integer, [Bus]) -> (Integer, Bus) -> (Integer, [Bus])
matchingSchedule (period, schedule) (busIndex, bus) = (newPeriod, newSchedule)
  where
    newOffset = find (\x -> (x + busIndex) `mod` bus == 0) schedule & fromMaybe bus
    newPeriod = if period == 0 then bus else lcm period bus
    newSchedule = [newOffset, newOffset + newPeriod ..]

day13part1 :: String -> String
day13part1 input = catMaybes busses
  & map (nextScheduleAfterMyDepart myDepart)
  & minimumBy (comparing snd)
  & (\(bus, depart) -> bus * (depart - myDepart))
  & show
  where (myDepart, busses) = parseInput input

day13part2 :: String -> String
day13part2 input = parseInput input
  & snd
  & indexBusses
  & foldl matchingSchedule (0, [])
  & snd
  & headDef 0
  & show
