module Common where

import           Control.Exception
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

intParser :: Parser Int
intParser = fromMaybe 0 . stringToInt <$> some digitChar

readListOf :: Parser a -> String -> [a]
readListOf parser = fromMaybe [] . parseMaybe (someTill (parser <* optional newline) eof)

stringToInt :: String -> Maybe Int
stringToInt s = read <$> parseMaybe parser s
  where parser = many digitChar :: Parser String

inputNotFound :: IOError -> IO String
inputNotFound _ = pure ""

readInput :: Int -> IO String
readInput day = readFile ("inputs/day" ++ show day ++ ".txt") `catch` inputNotFound
