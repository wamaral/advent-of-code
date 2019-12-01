module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

moduleParser :: Parser Int
moduleParser = fromMaybe 0 . stringToInt <$> some digitChar <* optional newline

parseModules :: String -> [Int]
parseModules = fromMaybe [] . parseMaybe (someTill moduleParser eof)

fuelRequired :: Int -> Int
fuelRequired m = (m `div` 3) - 2

day1part1 :: String -> String
day1part1 = show . sum . map fuelRequired . parseModules

day1part2 :: String -> String
day1part2 _input = "1-2"
