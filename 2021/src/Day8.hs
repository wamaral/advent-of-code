{-# LANGUAGE RecordWildCards #-}

module Day8
  (day8part1, day8part2)
  where

import           Common
import           Data.Function
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Display = Display {signals :: S.Set (S.Set Char), output :: [S.Set Char]} deriving (Show)

wireParser :: Parser Char
wireParser = oneOf "abcdefg"

displayParser :: Parser Display
displayParser = do
  signals <- S.fromList <$> (map S.fromList <$> someTill (some wireParser <* hspace) (string "| "))
  output <- map S.fromList <$> some wireParser `sepBy` hspace
  return Display{..}

fullDifference :: Ord a => S.Set a -> S.Set a -> S.Set a
fullDifference a b = S.union
  (S.difference (S.union a b) a)
  (S.difference (S.union a b) b)

solve :: Display -> Int
solve (Display signals output) = do
  let easy = IM.empty
        & IM.insert 1 (findByLength 2 signals)
        & IM.insert 4 (findByLength 4 signals)
        & IM.insert 7 (findByLength 3 signals)
        & IM.insert 8 (findByLength 7 signals)
  let dig9 = allByLength 6 signals & setFind (\x -> S.difference x (mapFind 4 easy) & S.size & (== 2))
  let segE = S.elemAt 0 $ S.difference (mapFind 8 easy) dig9
  let dig2 = allByLength 5 signals & setFind (S.member segE)
  let dig3 = allByLength 5 signals & setFind (\x -> fullDifference x dig2 & S.size & (== 2))
  let dig5 = allByLength 5 signals & setFind (\x -> fullDifference x dig2 & S.size & (== 4))
  let dig6 = allByLength 6 signals & setFind (\x -> S.difference x (S.insert segE dig5) == S.empty)
  let almost = easy
        & IM.insert 2 dig2
        & IM.insert 3 dig3
        & IM.insert 5 dig5
        & IM.insert 6 dig6
        & IM.insert 9 dig9
  let dig0 = S.elemAt 0 $ S.difference signals $ S.fromList (IM.elems almost)
  let digits = IM.insert 0 dig0 almost
  let dict = IM.toList digits & map (\(x,y) -> (y,x)) & M.fromList
  mapMaybe (`M.lookup` dict) output
    & reverse
    & zipWith (\x y -> y * (10 ^ x)) ([0 ..] :: [Int])
    & sum
  where
    mapFind i m = IM.lookup i m & fromMaybe S.empty
    setFind f s = S.filter f s & S.elemAt 0
    findByLength n = S.elemAt 0 . allByLength n
    allByLength n = S.filter (\x -> S.size x == n)

day8part1 :: String -> String
day8part1 = show . length . filter is1478 . concatMap output . readListOf displayParser
  where is1478 n = S.size n `elem` [2, 3, 4, 7]

day8part2 :: String -> String
day8part2 = show . sum . map solve . readListOf displayParser
