{-# LANGUAGE RecordWildCards #-}

module Day2
  (day2part1, day2part2)
  where

import           Common
import           Data.Function
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Policy = Policy
  { minimumCount :: Int
  , maximumCount :: Int
  , letter       :: Char
  , password     :: String
  }

policyParser :: Parser Policy
policyParser = do
  minimumCount <- intParser
  _ <- char '-'
  maximumCount <- intParser
  _ <- space
  letter <- letterChar
  _ <- string ": "
  password <- many letterChar
  return $ Policy{..}

isValid :: Policy -> Bool
isValid p = password p
  & filter (== letter p)
  & length
  & (\l -> l >= minimumCount p && l <= maximumCount p)

day2part1 :: String -> String
day2part1 = show . length . filter isValid . readListOf policyParser

day2part2 :: String -> String
day2part2 _ = ""
