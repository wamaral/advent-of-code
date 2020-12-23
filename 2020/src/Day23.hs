module Day23
  (day23part1, day23part2)
  where

import           Common
import           Data.Bifunctor
import           Data.Function
import           Data.List
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

inputParser :: Parser [Int]
inputParser = map (stringToInt0 . (: [])) <$> many digitChar <* optional newline <* eof

parseInput :: String -> [Int]
parseInput = fromMaybe [] . parseMaybe inputParser

step :: [Int] -> [Int]
step [] = []
step [_] = []
step [_,_] = []
step [_,_,_] = []
step [_,_,_,_] = [] -- Why did I enable all warnings?
step (x:a:b:c:xs) = before ++ [a,b,c] ++ after ++ [x]
  where
    destination = partition (< x) xs & bimap (maximumDef 0) (maximumDef 0) & (\(x',y') -> if x' > 0 then x' else y')
    (before, after) = breakAt destination xs

breakAt :: Int -> [Int] -> ([Int], [Int])
breakAt n xs = break (== n) xs & (\(xs',ys') -> (xs' ++ [n], tailDef [] ys'))

rotateTo :: Int -> [Int] -> [Int]
rotateTo n xs = break (== n) xs & (\(x, y) -> y ++ x)

day23part1 :: String -> String
day23part1 = intercalate "" . map show . tailDef [] . rotateTo 1 . lastDef [] . take (succ 100) . iterate step . parseInput

day23part2 :: String -> String
day23part2 _ = ""
