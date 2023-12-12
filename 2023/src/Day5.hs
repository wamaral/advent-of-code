{-# LANGUAGE RecordWildCards #-}

module Day5
  (day5part1, day5part2)
  where

import Common
import Data.Bifunctor
import Data.Function
import Data.List ( nub, sort, sortOn )
import Data.List.Split ( chunksOf )
import Data.Maybe
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char

data Input = Input { seeds :: [Seed], conversions :: [ConversionMap] } deriving Show
type Seed = Int
type SeedRange = (Seed, Seed)
type ConversionRange = (Seed, Seed)
data Conversion = Conversion { range :: ConversionRange, factor :: Int } deriving Show
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
  let range = (low, high)
  let factor = dest - low
  pure Conversion{..}

conversionMapParser :: Parser ConversionMap
conversionMapParser = do
  _ <- manyTill printChar newline
  sortOn (fst . range) <$> many conversionParser

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
applyConversion seed (Conversion (l, h) f) = if seed >= l && seed <= h then seed + f else seed

runConversionMap :: Seed -> ConversionMap -> Seed
runConversionMap seed conversions = map (applyConversion seed) conversions
  & filter (/= seed)
  & headDef seed

runSeed :: [ConversionMap] -> Seed -> Seed
runSeed cmap seed = foldl runConversionMap seed cmap

rangedSeeds :: [Seed] -> [SeedRange]
rangedSeeds seeds = chunksOf 2 seeds
  & map (\sds -> (headDef 0 sds, pred $ headDef 0 sds + lastDef 0 sds))

splitAtIntervals :: [Seed] -> SeedRange -> [SeedRange]
splitAtIntervals [] (l, h) = [(l, h)]
splitAtIntervals (i:is) (l, h) = (l, i) : splitAtIntervals is (succ i, h)

splitSeedRangesByConversionMap :: [SeedRange] -> ConversionMap -> [SeedRange]
splitSeedRangesByConversionMap sr cmap = sort $ concatMap (\sd -> splitAtIntervals (intervalsWithinRange sd) sd) sr
  where
    pairToA (a, b) = [a, b]
    conversionIntervals = nub $ concatMap (pairToA . range) cmap
    intervalsWithinRange (a, b) = filter (\i -> i > a && i < b) conversionIntervals

runSeedRanges :: [SeedRange] -> ConversionMap -> [SeedRange]
runSeedRanges sr cmap = splitSeedRangesByConversionMap sr cmap
  & map (bimap (`runConversionMap` cmap) (`runConversionMap` cmap))

runAllSeedRanges :: [ConversionMap] -> [SeedRange] -> [SeedRange]
runAllSeedRanges cs sr = foldl runSeedRanges sr cs

day5part1 :: String -> String
day5part1 input = seeds parsed
  & map (runSeed (conversions parsed))
  & minimum
  & show
  where
    parsed = parseInput input

day5part2 :: String -> String
day5part2 input = seeds parsed
  & rangedSeeds
  & runAllSeedRanges (conversions parsed)
  & sort
  & fmap fst
  & headDef 0
  & show
  where
    parsed = parseInput input
