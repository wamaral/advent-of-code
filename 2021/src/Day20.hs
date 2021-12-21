{-# LANGUAGE RecordWildCards #-}
module Day20
  (day20part1, day20part2)
  where

import           Common
import           Control.Lens
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V
import           Linear
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Pixel = Lit | Unlit deriving Eq
type Coord = V2 Int
type Lookup = V.Vector Pixel
data Image = Image {pixels :: M.Map Coord Pixel, background :: Pixel} deriving Show

instance Show Pixel where
  show Lit   = "#"
  show Unlit = "."

pixelParser :: Parser Pixel
pixelParser = choice
  [ Lit <$ char '#'
  , Unlit <$ char '.'
  ]

lookupParser :: Parser Lookup
lookupParser = V.fromList <$> (some pixelParser <* optional newline)

imageParser :: Parser Image
imageParser = do
  pixels <- listOfListsToV2Map <$> some (some pixelParser <* optional newline)
  return $ Image {background = Unlit, ..}

inputParser :: Parser (Lookup, Image)
inputParser = do
  l <- lookupParser
  _ <- newline
  i <- imageParser
  _ <- eof
  return (l, i)

parseInput :: String -> (Lookup, Image)
parseInput = fromMaybe (V.empty, Image M.empty Unlit) . parseMaybe inputParser

neighbourhood :: [Coord]
neighbourhood = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                 V2 (-1) 0,    V2 0 0,    V2 1 0,
                 V2 (-1) 1,    V2 0 1,    V2 1 1]

squareCoords :: Coord -> [Coord]
squareCoords p = map (+ p) neighbourhood

squareToInt :: [Pixel] -> Int
squareToInt sq = reverse sq & zip ([0..] :: [Int]) & foldl' (\n (i,coord) -> if coord == Unlit then n else n + (2 ^ i)) 0

grow :: Image -> Image
grow image = do
  let u = map (\x -> V2 x (newMin ^._y)) [newMin ^._x .. newMax ^._x] & fill
  let d = map (\x -> V2 x (newMax ^._y)) [newMin ^._x .. newMax ^._x] & fill
  let l = map (\y -> V2 (newMin ^._x) y) [newMin ^._y .. newMax ^._y] & fill
  let r = map (\y -> V2 (newMax ^._x) y) [newMin ^._y .. newMax ^._y] & fill
  image {pixels = M.unions [pixels image, u, d, l, r]}
  where
    fill pxs = zip pxs (repeat (background image)) & M.fromList
    (min', max') = M.keys (pixels image) & (\xs -> (head xs, last xs))
    newMin = min' + V2 (-1) (-1)
    newMax = max' + V2 1 1

enhance :: Lookup -> Image -> Image
enhance lkp image = Image {pixels = newPixels, background = newBackground}
  where
    grown = grow $ grow image
    newVal p = squareCoords p
      & map (fromMaybe (background grown) . (`M.lookup` pixels grown))
      & squareToInt
      & (V.!) lkp
    newPixels = M.mapWithKey (\k _ -> newVal k) (pixels grown)
    newBackground = if background image == Lit then V.last lkp else V.head lkp

-- showImage :: Image -> String
-- showImage image = map (\y -> concatMap (\x -> maybe " " show (M.lookup (V2 x y) (pixels image))) [min' ^._x .. max' ^._x]) [min' ^._y .. max' ^._y] & unlines
--   where (min', max') = M.keys (pixels image) & (\xs -> (head xs, last xs))

enhanceN :: Int -> Lookup -> Image -> Image
enhanceN n lkp image = iterate (enhance lkp) image & take (succ n) & last

day20part1 :: String -> String
day20part1 input = baseImage
  & enhanceN 2 lkp
  & pixels
  & M.filter (== Lit)
  & M.size
  & show
  where (lkp, baseImage) = parseInput input

day20part2 :: String -> String
day20part2 input = baseImage
  & enhanceN 50 lkp
  & pixels
  & M.filter (== Lit)
  & M.size
  & show
  where (lkp, baseImage) = parseInput input
