{-# LANGUAGE RecordWildCards #-}

module Day12
  (day12part1, day12part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Set             as S
import           Data.Tuple
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Position = V2 Int
type Elevation = Int
type TerrainMap = M.Map Position Elevation
type TerrainPaths = M.Map Position [Position]
data Terrain = Terrain
  { terrainMap     :: TerrainMap
  , terrainPaths   :: TerrainPaths
  , startPosition  :: Position
  , targetPosition :: Position
  } deriving Show

elevationParser :: Parser Elevation
elevationParser = toElevation <$> (lowerChar <|> char 'S' <|> char 'E')
  where
    toElevation 'S' = 0
    toElevation 'E' = 27
    toElevation x   = fromMaybe 99 $ lookup x $ zip ['a'..'z'] [1..]

elevationParserReverse :: Parser Elevation
elevationParserReverse = toElevation <$> (lowerChar <|> char 'S' <|> char 'E')
  where
    toElevation 'S' = 27
    toElevation 'E' = 0
    toElevation x   = fromMaybe 99 $ lookup x $ zip ['z','y'..'a'] [1..]

parseInput :: Parser Elevation -> String -> Terrain
parseInput evParser input = do
  let terrainMap' = listOfListsToV2Map $ readListOf (many evParser) input
  let boundaries = M.filter (\e -> e == 0 || e == 27) terrainMap' & M.assocs & map swap & sort & map snd
  let startPosition = head boundaries
  let targetPosition = last boundaries
  let terrainMap = M.adjust pred targetPosition terrainMap' & M.adjust succ startPosition
  let terrainPaths = terrainMapToPaths terrainMap
  Terrain{..}

neighbours :: Position -> [Position]
neighbours x = map (+ x) [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

possibles :: TerrainMap -> Position -> TerrainMap
possibles t p = neighbours p
  & mapMaybe (switcheroo . (\p' -> (p', M.lookup p' t)))
  & filter ((<= succ curElevation) . snd)
  & M.fromList
  where
    curElevation = fromMaybe 99 $ M.lookup p t
    switcheroo (a, Just b)  = Just (a, b)
    switcheroo (_, Nothing) = Nothing

terrainMapToPaths :: TerrainMap -> TerrainPaths
terrainMapToPaths t = M.mapWithKey (\k _ -> M.keys $ possibles t k) t

shortestCount :: Terrain -> S.Set Position -> S.Set Position -> Int -> Int
shortestCount t toCheck visited cnt = do
  let newToCheck = S.elems toCheck & mapMaybe (`M.lookup` terrainPaths t) & concat & filter (`S.notMember` visited) & S.fromList
  let canSeeTarget = (== 26) $ S.findMax $ S.map (fromMaybe 0 . (`M.lookup` terrainMap t)) toCheck
  if canSeeTarget
    then cnt
    else shortestCount t newToCheck (S.union visited toCheck) (cnt + 1)

day12part1 :: String -> String
day12part1 input = show $ shortestCount terrain (S.singleton (startPosition terrain)) S.empty 1
  where terrain = parseInput elevationParser input

day12part2 :: String -> String
day12part2 input = show $ shortestCount terrain (S.singleton (startPosition terrain)) S.empty 0
  where terrain = parseInput elevationParserReverse input
