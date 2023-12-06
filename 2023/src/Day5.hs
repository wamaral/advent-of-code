{-# LANGUAGE RecordWildCards #-}

module Day5
  (day5part1, day5part2)
  where

import Common
import Data.Function
import Data.Maybe
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char

data Input = Input { seeds :: [Seed], conversions :: [ConversionMap] } deriving Show
type Seed = Int
data Conversion = Conversion { low :: Int, high :: Int, factor :: Int } deriving Show
type ConversionMap = [Conversion]

conversionParser :: Parser Conversion
conversionParser = do
  dest <- intParser
  _ <- hspace
  low <- intParser
  _ <- hspace
  len <- intParser
  _ <- optional newline
  let high = low + len - 1
  let factor = dest - low
  pure Conversion{..}

conversionMapParser :: Parser ConversionMap
conversionMapParser = do
  _ <- manyTill printChar newline
  many conversionParser

inputParser :: Parser Input
inputParser = do
  _ <- string "seeds: "
  seeds <- many (intParser <* optional hspace)
  _ <- newline
  _ <- newline
  conversions <- many (conversionMapParser <* optional newline)
  _ <- eof
  pure Input{..}

parseInput :: String -> Input
parseInput = fromMaybe Input{ seeds = [], conversions = [] } . parseMaybe inputParser

applyConversion :: Seed -> Conversion -> Seed
applyConversion seed (Conversion l h f) = if seed >= l && seed <= h then seed + f else seed

runConversionMap :: Seed -> ConversionMap -> Seed
runConversionMap seed conversions = map (applyConversion seed) conversions
  & filter (/= seed)
  & headDef seed

runSeed :: [ConversionMap] -> Seed -> Seed
runSeed cmap seed = foldl runConversionMap seed cmap

day5part1 :: String -> String
day5part1 input = seeds parsed
  & map (runSeed (conversions parsed))
  & minimum
  & show
  where
    parsed = parseInput input

day5part2 :: String -> String
day5part2 _ = ""
