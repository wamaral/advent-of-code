module Day3
  (day3part1, day3part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Sack = (String, String)

sackParser :: Parser Sack
sackParser = do
  chars <- some letterChar <* optional newline
  pure $ splitAt (length chars `div` 2) chars

priority :: Char -> Int
priority c = zip (['a' .. 'z'] ++ ['A'..'Z']) [1 ..]
  & find (\(a,_) -> a == c)
  & maybe 0 snd

day3part1 :: String -> String
day3part1 = show . sum . map (priority . headDef ' ' . uncurry intersect) . readListOf sackParser

day3part2 :: String -> String
day3part2 _ = ""
