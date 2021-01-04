module Day25
  (day25part1, day25part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

inputParser :: Parser (Int, Int)
inputParser = do
  cardPub <- intParser
  _ <- newline
  doorPub <- intParser
  _ <- optional newline
  _ <- eof
  return (cardPub, doorPub)

parseInput :: String -> (Int, Int)
parseInput = fromMaybe (0,0) . parseMaybe inputParser

transform :: Int -> Int -> Int
transform subject value = (value * subject) `rem` 20201227

transforms :: Int -> [Int]
transforms subject = tail $ iterate (transform subject) 1

loopCounts :: (Int, Int) -> (Int, Int)
loopCounts (cardPub, doorPub) = (getCount cardPub, getCount doorPub)
  where getCount pub = elemIndex pub (transforms 7) & fmap succ & fromMaybe 0

encryptionKey :: (Int, Int) -> Int
encryptionKey pubs@(_, doorPub) = transforms doorPub & take cardLoop & lastDef 0
  where (cardLoop, _) = loopCounts pubs

day25part1 :: String -> String
day25part1 = show . encryptionKey . parseInput

day25part2 :: String -> String
day25part2 _ = ""
