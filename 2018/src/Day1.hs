module Day1
  (day1part1, day1part2)
  where

import           Common
import           Data.List
import           Data.Maybe
import qualified Data.Set             as Set
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Operation = Plus | Minus
data Frequency = Frequency Operation Int

frequencyParser :: Parser Frequency
frequencyParser = do
  sign <- char '+' <|> char '-'
  nums <- stringToInt <$> some digitChar
  optional newline
  let op = if sign == '+' then Plus else Minus
  pure $ Frequency op (fromMaybe 0 nums)

allFrequencies :: String -> [Frequency]
allFrequencies input = fromMaybe [] $ parseMaybe (someTill frequencyParser eof) input

foldFrequencies :: Int -> Frequency -> Int
foldFrequencies count (Frequency op x) = case op of
  Plus  -> count + x
  Minus -> count - x

firstDup :: [Int] -> Set.Set Int -> Maybe Int
firstDup [] _ = Nothing
firstDup (x:xs) set = if Set.member x set then Just x else firstDup xs (Set.insert x set)

day1part1 :: String -> String
day1part1 input = show $ foldl' foldFrequencies 0 $ allFrequencies input

day1part2 :: String -> String
day1part2 input = case firstDup frequencySteps Set.empty of
    Nothing -> "Not found"
    Just x  -> show x
  where frequencySteps = scanl' foldFrequencies 0 $ cycle (allFrequencies input)
