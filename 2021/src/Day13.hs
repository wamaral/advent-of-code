module Day13
  (day13part1, day13part2)
  where

import           Common
import           Control.Lens         hiding (Fold)
import           Data.List
import           Data.Maybe
import qualified Data.Set             as S
import           Linear.V2
import           Linear.Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Dot = V2 Int
type Dots = S.Set Dot
type Fold = V2 Int

dotParser :: Parser Dot
dotParser = do
  x <- intParser
  _ <- char ','
  y <- intParser
  return $ V2 x y

foldParser :: Parser Fold
foldParser = do
  _ <- string "fold along "
  xy <- oneOf "xy"
  _ <- char '='
  n <- intParser
  return $ if xy == 'x' then V2 n 0 else V2 0 n

inputParser :: Parser (Dots, [Fold])
inputParser = do
  dots <- some (dotParser <* optional newline)
  _ <- newline
  folds <- some (foldParser <* optional newline)
  _ <- eof
  return (S.fromList dots, folds)

parseInput :: String -> (Dots, [Fold])
parseInput = fromMaybe (S.empty, []) . parseMaybe inputParser

foldDots :: Dots -> Fold -> Dots
foldDots dots fold = S.map (\dot -> if toFold dot then dot - (diff dot & normalize & (* 2)) else dot) dots
  where
    toFold dot = dot ^._x >= fold ^._x && dot ^._y >= fold ^._y
    diff dot = dot ^-^ fold
    normalize dot = if fold ^._x == 0 then dot * V2 0 1 else dot * V2 1 0

day13part1 :: String -> String
day13part1 input = foldl' foldDots dots (take 1 folds)
  & S.size
  & show
  where (dots, folds) = parseInput input

day13part2 :: String -> String
day13part2 _ = ""
