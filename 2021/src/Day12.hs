module Day12
  (day12part1, day12part2)
  where

import           Common
import           Data.Function
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Cave = Start | End | Small String | Big String deriving (Eq, Ord, Show)

type CaveMap = M.Map Cave [Cave]

data CaveTree = Branch Cave [CaveTree] deriving Show

caveParser :: Parser Cave
caveParser = choice
  [ Start <$ string "start"
  , End <$ string "end"
  , Small <$> some lowerChar
  , Big <$> some upperChar
  ]

cavePathParser :: Parser (Cave, Cave)
cavePathParser = do
  cave1 <- caveParser
  _ <- char '-'
  cave2 <- caveParser
  return (cave1, cave2)

mkMap :: [(Cave, Cave)] -> CaveMap
mkMap = foldl' (\m (a, b) -> M.insertWith (++) a (filter (not . isStart) [b]) m & M.insertWith (++) b (filter (not . isStart) [a])) M.empty

mkTree :: CaveMap -> CaveTree
mkTree caveMap = go [] Start
  where
    go :: [Cave] -> Cave -> CaveTree
    go _ End = Branch End []
    go visited cave = do
      let toVisit = M.lookup cave caveMap & fromMaybe [] & filter (`notElem` visited)
      let newVisited = if nonRevisitable cave then cave:visited else visited
      Branch cave (map (go newVisited) toVisit)

isStart :: Cave -> Bool
isStart Start = True
isStart _     = False

nonRevisitable :: Cave -> Bool
nonRevisitable (Small _) = True
nonRevisitable _         = False

allPaths :: CaveTree -> [[Cave]]
allPaths caveTree = paths [] caveTree & map reverse
  where
    paths :: [Cave] -> CaveTree -> [[Cave]]
    paths ps (Branch c [])       = [c:ps]
    paths ps (Branch c subTrees) = concatMap (paths (c:ps)) subTrees

isCompletePath :: [Cave] -> Bool
isCompletePath = (== End) . last

day12part1 :: String -> String
day12part1 = show . length . filter isCompletePath . allPaths . mkTree . mkMap . readListOf cavePathParser

day12part2 :: String -> String
day12part2 _ = ""
