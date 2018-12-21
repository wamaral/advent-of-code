module Day1
  (day1part1)
  where

import           Common
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Operation = Plus | Minus deriving Show
data Frequency = Frequency Operation Int deriving Show

frequencyParser :: Parser Frequency
frequencyParser = do
  sign <- char '+' <|> char '-'
  nums <- stringToInt <$> some digitChar
  optional newline
  let op = if sign == '+' then Plus else Minus
  pure $ Frequency op (fromMaybe 0 nums)

allFrequencies :: String -> [Frequency]
allFrequencies input = fromMaybe [] $ parseMaybe (someTill frequencyParser eof) input

day1part1 :: String -> String
day1part1 input = show $ foldl sumFreq 0 $ allFrequencies input
  where
    sumFreq count (Frequency op x) = case op of
      Plus  -> count + x
      Minus -> count - x
