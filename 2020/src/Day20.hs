{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Day20
  (day20part1, day20part2)
  where

import           Common
import           Control.Monad.State
import           Data.Function
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Linear.V2
import           Linear.Vector
import           Safe
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

data Pixel = On | Off deriving (Eq, Show)
type Coord = V2 Int
type TileBoard = M.Map Coord Pixel
data BorderOrientation = BTop | BRight | BBottom | BLeft | BTop' | BRight' | BBottom' | BLeft' deriving (Bounded, Enum, Eq, Ord, Show)
type Border = [Pixel]
type Borders = M.Map BorderOrientation Border
type WorldMap = M.Map Coord Tile

data WorldBuilder = WorldBuilder
  { worldMap       :: WorldMap
  , availableTiles :: [Tile]
  , coordsToCheck  :: [Coord]
  }

data Tile = Tile
  { tileId      :: Int
  , tileBoard   :: TileBoard
  , tileBorders :: Borders
  } deriving Show

instance Eq Tile where
  t1 == t2 = tileId t1 == tileId t2

pixelParser :: Parser Pixel
pixelParser = choice
  [ On <$ char '#'
  , Off <$ char '.'
  ]

tileBoardParser :: Parser TileBoard
tileBoardParser = do
  board <- many pixelParser `sepEndBy` newline
  let indexedBoard = zip [9,8..0] $ map (zip [0..]) board
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

borderVectors :: BorderOrientation -> [Coord]
borderVectors BTop     = map (`V2` 9) [0..9]
borderVectors BRight   = map (V2 9) [0..9]
borderVectors BBottom  = map (`V2` 0) [0..9]
borderVectors BLeft    = map (V2 0) [0..9]
borderVectors BTop'    = reverse $ borderVectors BTop
borderVectors BRight'  = reverse $ borderVectors BRight
borderVectors BBottom' = reverse $ borderVectors BBottom
borderVectors BLeft'   = reverse $ borderVectors BLeft

allBorders :: TileBoard -> Borders
allBorders board = foldl' getBorder M.empty ([minBound..maxBound] :: [BorderOrientation])
  where
    getBorder m orientation = M.insert orientation (mapMaybe (`M.lookup` board) (borderVectors orientation)) m

bordersMatchingOthers :: [Tile] -> Tile -> Borders
bordersMatchingOthers allTiles tile = M.filter borderInOtherTiles (tileBorders tile)
  where
    otherTiles = filter (/= tile) allTiles
    borderInOtherTiles border = any (\t -> border `elem` M.elems (tileBorders t)) otherTiles

-- Rotate, flip
adjustTile :: (Coord -> Coord) -> Tile -> Tile
adjustTile f t = t {tileBoard = newBoard, tileBorders = allBorders newBoard}
  where newBoard = M.mapKeys f (tileBoard t) & translateToOrigin

translateToOrigin :: M.Map Coord a -> M.Map Coord a
translateToOrigin m = M.mapKeys (+ translateVec) m
  where translateVec = M.lookupMin m & fmap fst & fmap rotateVector180 & fromMaybe (V2 0 0)

rotateVectorCw, rotateVectorCcw, rotateVector180 :: Coord -> Coord
rotateVectorCw (V2 x y) = V2 y (negate x)
rotateVectorCcw = perp
rotateVector180 (V2 x y) = V2 (negate x) (negate y)

rotateTileCw, rotateTileCcw, rotateTile180 :: Tile -> Tile
rotateTileCw = adjustTile rotateVectorCw
rotateTileCcw = adjustTile rotateVectorCcw
rotateTile180 = adjustTile rotateVector180

flipVectorH, flipVectorV :: Coord -> Coord
flipVectorH (V2 x y) = V2 x (9 - y)
flipVectorV (V2 x y) = V2 (9 - x) y

flipTileH, flipTileV :: Tile -> Tile
flipTileH = adjustTile flipVectorH
flipTileV = adjustTile flipVectorV

matchOrientation :: BorderOrientation -> BorderOrientation -> (Tile -> Tile)
matchOrientation o1 o2 = zip [minBound .. maxBound] fns & lookupJustDef id o2
  where
    fns = case o1 of
      BTop     -> [flipTileH, rotateTileCw, id, flipTileV . rotateTileCcw, rotateTile180, flipTileV . rotateTileCw, flipTileV, rotateTileCcw]
      BRight   -> [rotateTileCcw, flipTileV, flipTileH . rotateTileCw, id, flipTileH . rotateTileCcw, rotateTile180, rotateTileCw, flipTileH]
      BBottom  -> [id, flipTileV . rotateTileCcw, flipTileH, rotateTileCw, flipTileV, rotateTileCcw, rotateTile180, flipTileV . rotateTileCw]
      BLeft    -> [flipTileH . rotateTileCw, id, rotateTileCcw, flipTileV, rotateTileCw, flipTileH, flipTileH . rotateTileCcw, rotateTile180]
      BTop'    -> [rotateTile180, flipTileV . rotateTileCw, flipTileV, rotateTileCcw, flipTileH, rotateTileCw, id, flipTileV . rotateTileCcw]
      BRight'  -> [flipTileH . rotateTileCcw, rotateTile180, rotateTileCw, flipTileH, rotateTileCcw, flipTileV, flipTileH . rotateTileCw, id]
      BBottom' -> [flipTileV, rotateTileCcw, rotateTile180, flipTileV . rotateTileCw, id, flipTileV . rotateTileCcw, flipTileH, rotateTileCw]
      BLeft'   -> [rotateTileCw, flipTileH, flipTileH . rotateTileCcw, rotateTile180, flipTileH . rotateTileCw, id, rotateTileCcw, flipTileV]

-- State
startingWorld :: [Tile] -> WorldBuilder
startingWorld tiles = WorldBuilder
  { worldMap = M.insert (V2 0 0) startingTile M.empty
  , availableTiles = remainingTiles
  , coordsToCheck = [V2 0 0]
  }
  where
    startingTile = head $ filter ((== 4) . M.size . bordersMatchingOthers tiles) tiles
    remainingTiles = filter (/= startingTile) tiles

buildWorld :: State WorldBuilder ()
buildWorld = do
  wb <- get
  if null (coordsToCheck wb) then return () else do
    let checkingCoord = head $ coordsToCheck wb -- We know it's not empty, so it's safe
    let checkingTile = M.lookup checkingCoord (worldMap wb) & fromJust -- I guess safe too?
    let placesToAssign = [(BTop, V2 0 1), (BRight, V2 1 0), (BBottom, V2 0 (-1)), (BLeft, V2 (-1) 0)] & map (fmap (+ checkingCoord)) & filter (\(_, v) -> M.notMember v (worldMap wb))
    let foundTiles = mapMaybe (tileToAssign checkingTile (availableTiles wb)) placesToAssign
    let newMap = foldl' (\m (place, t) -> M.insert place t m) (worldMap wb) foundTiles
    let newTiles = availableTiles wb \\ map snd foundTiles
    let newCoords = tail (coordsToCheck wb) ++ map fst foundTiles
    put WorldBuilder {worldMap = newMap, availableTiles = newTiles, coordsToCheck = newCoords}
    buildWorld
  where
    tileToAssign baseTile avTiles (orient, newPlace) = findMatchingTile (M.lookup orient (tileBorders baseTile) & fromJust) avTiles
      & fmap (\(matchOrient, matchTile) -> (newPlace, matchOrientation orient matchOrient matchTile))

findMatchingTile :: Border -> [Tile] -> Maybe (BorderOrientation, Tile)
findMatchingTile border tiles = matchingTile
  & fmap (\t -> M.filter (== border) (tileBorders t) & M.keys & head)
  & fmap (, fromJust matchingTile) -- We know matchingTile is Just here, so it's safe
  where matchingTile = find (\t -> border `elem` M.elems (tileBorders t)) tiles

mergeWorldMap :: WorldMap -> TileBoard
mergeWorldMap wm = M.map dropBorders wm
  & M.mapWithKey translateToWorld
  & M.map M.toList
  & M.elems
  & concat
  & M.fromList
  where
    dropBorders tile = M.difference (tileBoard tile) borderVecsToRemove & translateToOrigin
    borderVecsToRemove = [BTop .. BLeft] & concatMap borderVectors & (\x -> zip x x) & M.fromList
    translateToWorld vec tb = M.mapKeys (+ (vec ^* 8)) tb

--                   #
-- #    ##    ##    ###
--  #  #  #  #  #  #
monsterVec :: [Coord]
monsterVec =
  [ V2 18 2
  , V2 0 1, V2 5 1, V2 6 1, V2 11 1, V2 12 1, V2 17 1, V2 18 1, V2 19 1
  , V2 1 0, V2 4 0, V2 7 0, V2 10 0, V2 13 0, V2 16 0]

isMonsterHere :: TileBoard -> Coord -> Bool
isMonsterHere tb vec = all ((== Just On) . (`M.lookup` tb)) monsterMask
  where monsterMask = map (+ vec) monsterVec

monsterCount :: TileBoard -> (Int, TileBoard)
monsterCount tb = M.filterWithKey (\k _ -> isMonsterHere tb k) tb & M.size & (,tb)

day20part1 :: String -> String
day20part1 input = show $ product $ map tileId $ filter ((== 4) . M.size . bordersMatchingOthers tiles) tiles
  where tiles = readListOf tileParser input

day20part2 :: String -> String
day20part2 input = map monsterCount allMaps
  & find (\(monsters, _) -> monsters > 0)
  & fmap (\(monsters, tb) -> M.filter (== On) tb & M.size & (\x -> x - (monsters * length monsterVec)))
  & fromMaybe 0
  & show
  where
    tiles = readListOf tileParser input
    fullMap = mergeWorldMap $ translateToOrigin $ worldMap $ execState buildWorld $ startingWorld tiles
    allRotations = iterate (M.mapKeys rotateVectorCcw) fullMap & take 4 & map translateToOrigin
    allMaps = allRotations ++ (map (M.mapKeys flipVectorH) allRotations & map translateToOrigin)
