{-# LANGUAGE RecordWildCards #-}

module Day20
  (day20part1, day20part2)
  where

import           Common
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Pixel = On | Off deriving (Eq, Show)
type Coord = V2 Int
type TileBoard = M.Map Coord Pixel

data Tile = Tile
  { tileId      :: Int
  , tileBoard   :: TileBoard
  , tileBorders :: [[Pixel]]
  } deriving Show

pixelParser :: Parser Pixel
pixelParser = choice
  [ On <$ char '#'
  , Off <$ char '.'
  ]

tileBoardParser :: Parser TileBoard
tileBoardParser = do
  board <- many pixelParser `sepEndBy` newline
  let indexedBoard = zip [0..] $ map (zip [0..]) board
  let tileBoard = foldl' (\m (y, pixels) -> foldl' (\m' (x, pixel) -> M.insert (V2 x y) pixel m') m pixels) M.empty indexedBoard
  return tileBoard

tileParser :: Parser Tile
tileParser = do
  _ <- string "Tile "
  tileId <- intParser
  _ <- char ':'
  _ <- newline
  tileBoard <- tileBoardParser
  let tileBorders = allBorders tileBoard
  return Tile{..}

allBorders :: TileBoard -> [[Pixel]]
allBorders board = map getBorder ([v1, v2, v3, v4] ++ map reverse [v1, v2, v3, v4])
  where
    getBorder = mapMaybe (`M.lookup` board)
    v1 = map (V2 0) [0..9]
    v2 = map (`V2` 0) [0..9]
    v3 = map (V2 9) [0..9]
    v4 = map (`V2` 9) [0..9]

bordersMatchingOthers :: [Tile] -> Tile -> [[Pixel]]
bordersMatchingOthers allTiles tile = filter borderInOtherTiles (tileBorders tile)
  where
    otherTiles = filter (\t -> tileId t /= tileId tile) allTiles
    borderInOtherTiles border = any (\t -> border `elem` tileBorders t) otherTiles

day20part1 :: String -> String
day20part1 input = show $ product $ map tileId $ filter ((== 4) . length . bordersMatchingOthers tiles) tiles
  where tiles = readListOf tileParser input

day20part2 :: String -> String
day20part2 _ = ""
