{-# LANGUAGE RecordWildCards #-}

module Day2
  (day2part1, day2part2)
  where

import           Common
import           Data.Function
import           Data.Maybe
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Policy = Policy
  { lower    :: Int
  , higher   :: Int
  , letter   :: Char
  , password :: String
  }

policyParser :: Parser Policy
policyParser = do
  lower <- intParser
  _ <- char '-'
  higher <- intParser
  _ <- space
  letter <- letterChar
  _ <- string ": "
  password <- many letterChar
  return $ Policy{..}

isValidPart1 :: Policy -> Bool
isValidPart1 p = password p
  & filter (== letter p)
  & length
  & (\l -> l >= lower p && l <= higher p)

isValidPart2 :: Policy -> Bool
isValidPart2 p = [(lower p), (higher p)]
  & map pred
  & mapMaybe (atMay (password p))
  & filter (== letter p)
  & length
  & (== 1)

day2part1 :: String -> String
day2part1 = show . length . filter isValidPart1 . readListOf policyParser

day2part2 :: String -> String
day2part2 = show . length . filter isValidPart2 . readListOf policyParser
