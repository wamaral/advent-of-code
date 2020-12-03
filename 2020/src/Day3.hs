module Day3
  (day3part1, day3part2)
  where

import           Common
import           Data.Function
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Square = Open | Tree deriving (Eq, Show)

squareParser :: Parser Square
squareParser = choice
  [ Open <$ char '.'
  , Tree <$ char '#'
  ]

squareAtPos :: Int -> [Square] -> Maybe Square
squareAtPos n squares = atMay (cycle squares) n

squaresBySlope :: Int -> Int -> [[Square]] -> [Square]
squaresBySlope x y area = area
  & iterate (drop y)
  & takeWhile (not . null)
  & map head
  & tail -- We have the list of rows after skipping every Y
  & zip [x, x*2..]
  & mapMaybe (uncurry squareAtPos)

treeCountBySlope :: Int -> Int -> [[Square]] -> Int
treeCountBySlope x y area = squaresBySlope x y area & filter (== Tree) & length

day3part1 :: String -> String
day3part1 = show . treeCountBySlope 3 1 . readListOf (many squareParser)

day3part2 :: String -> String
day3part2 input = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  & map (\(x,y) -> treeCountBySlope x y area)
  & product
  & show
  where area = readListOf (many squareParser) input
