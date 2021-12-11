module Day10
  (day10part1, day10part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Stack
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

validChars :: String
validChars = "<([{}])>"

inputParser :: Parser String
inputParser = some $ oneOf validChars <* hspace

pair :: Char -> Char
pair c = zip validChars (reverse validChars) & lookupJustDef ' ' c

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _   = 0

points2 :: Char -> Int
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4
points2 _   = 0

calc2 :: String -> Int
calc2 = foldl' (\acc c -> acc * 5 + points2 c) 0

unwind :: Stack Char -> String
unwind = go ""
  where go xs st = if stackIsEmpty st then xs else stackPop st & fromJust & (\(a,b) -> go (xs ++ [b]) a)

processLine :: String -> (Stack Char, String)
processLine = go stackNew
  where
    go st [] = (st, [])
    go st (x:xs)
      | x `elem` "<([{" = go (stackPush st x) xs
      | stackPeek st == Just (pair x) = go (maybe stackNew fst (stackPop st)) xs
      | otherwise = (st, x:xs)

day10part1 :: String -> String
day10part1 input = readListOf inputParser input
  & map (snd . processLine)
  & filter (not . null)
  & map (points . headDef ' ')
  & sum
  & show

day10part2 :: String -> String
day10part2 input = readListOf inputParser input
  & map processLine
  & filter (null . snd)
  & map (calc2 . map pair . unwind . fst)
  & middle
  & show
  where middle xs = sort xs !! (length xs `div` 2)
