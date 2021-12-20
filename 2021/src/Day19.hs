{-# LANGUAGE RecordWildCards #-}

module Day19
  (day19part1, day19part2)
  where

import           Common
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set             as S
import           Linear
import           Text.Megaparsec      hiding (match, region)
import           Text.Megaparsec.Char

type Coord = V3 Int
type Region = S.Set Coord

data Scanner = Scanner
  { position :: Maybe Coord
  , rotated  :: Maybe Region
  , beacons  :: Region
  } deriving Show

coordParser :: Parser Coord
coordParser = V3
  <$> (signedIntParser <* char ',')
  <*> (signedIntParser <* char ',')
  <*> signedIntParser


scannerParser :: Parser Scanner
scannerParser = do
  _ <- string "--- scanner "
  _ <- intParser
  _ <- string " ---"
  _ <- newline
  beacons <- S.fromList <$> many (coordParser <* optional newline)
  return Scanner{position = Nothing, rotated = Nothing, ..}

inputParser :: Parser [Scanner]
inputParser = many (scannerParser <* optional newline)

parseInput :: String -> [Scanner]
parseInput = fromMaybe [] . parseMaybe inputParser

yaw :: Coord -> Coord
yaw (V3 x y z) = V2 x z & perp & (\(V2 x' z') -> V3 x' y z')

pitch :: Coord -> Coord
pitch (V3 x y z) = V2 y z & perp & (\(V2 y' z') -> V3 x y' z')

roll :: Coord -> Coord
roll (V3 x y z) = V2 x y & perp & (\(V2 x' y') -> V3 x' y' z)

allOrientations :: Region -> [Region]
allOrientations cs = iterate (S.map roll) cs
  & take 4
  & (++ (iterate (S.map yaw) cs & (\ys -> [ys !! 1, ys !! 3])))
  & concatMap (take 4 . iterate (S.map pitch))

matchRegion :: Region -> Region -> Maybe (Coord, Region)
matchRegion known query = S.map (\knownCoord -> S.map (checkPoint knownCoord) query & S.toList & msum) known & S.toList & msum
  where
    checkPoint knownCoord queryCoord = do
      let diffCoord = knownCoord ^-^ queryCoord
      let translated = S.map (+ diffCoord) query
      if S.intersection known translated & S.size & (>= 12) then Just (diffCoord, translated) else Nothing

match :: Region -> Scanner -> (Region, Scanner)
match knownRegion scanner@(Scanner _ _ beacons') = case map (matchRegion knownRegion) (allOrientations beacons') & msum of
    Nothing -> (knownRegion, scanner)
    Just (pos, translated) -> (S.union knownRegion translated, Scanner{position = Just pos, rotated = Just translated, beacons = beacons'})

matchAll :: [Scanner] -> (Region, [Scanner])
matchAll [] = (S.empty, [])
matchAll (firstScanner:restScanners) = go (beacons baseScanner) (baseScanner : restScanners)
  where
    baseScanner = Scanner {position = Just (V3 0 0 0), rotated = Just (beacons firstScanner), beacons = beacons firstScanner}
    isMatched (Scanner p _ _) = isJust p
    matchScanner :: (Region, [Scanner]) -> Scanner -> (Region, [Scanner])
    matchScanner (region, scanners) scannerToMatch = if isMatched scannerToMatch
      then (region, scannerToMatch : scanners)
      else match region scannerToMatch & \(r, s) -> (r, s : scanners)
    go :: Region -> [Scanner] -> (Region, [Scanner])
    go region scanners = do
      let (newRegion, newScanners) = foldl' matchScanner (region, []) scanners
      if all isMatched newScanners then (newRegion, newScanners) else go newRegion newScanners

day19part1 :: String -> String
day19part1 = show . S.size . fst . matchAll . parseInput

day19part2 :: String -> String
day19part2 _ = ""
