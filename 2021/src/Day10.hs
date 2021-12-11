module Day10
  (day10part1, day10part2)
  where

import           Common
import           Data.Function
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

processLine :: String -> (Stack Char, String)
processLine = go stackNew
  where
    go st [] = (st, [])
    go st (x:xs)
      | x `elem` "<([{" = go (stackPush st x) xs
      | stackPeek st == Just (pair x) = go (maybe stackNew fst (stackPop st)) xs
      | otherwise = (st, x:xs)

day10part1 :: String -> String
day10part1 = show . sum . map (points . headDef ' ') . filter (not . null) . map (snd . processLine) . readListOf inputParser

day10part2 :: String -> String
day10part2 _ = ""
