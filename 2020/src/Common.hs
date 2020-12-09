module Common where

import           Control.Exception
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

intParser :: Parser Int
intParser = stringToInt0 <$> some digitChar

signedIntParser :: Parser Int
signedIntParser = do
  sign <- optional $ char '-' <|> char '+'
  int <- stringToInt0 <$> some digitChar
  return $ if sign == Just '-' then negate int else int

linesParser :: Parser a -> Parser [a]
linesParser parser = someTill (parser <* optional newline) eof

readListOf :: Parser a -> String -> [a]
readListOf parser = fromMaybe [] . parseMaybe (linesParser parser)

readListOfDebug :: Show a => Parser a -> String -> String
readListOfDebug parser = either errorBundlePretty show . parse (linesParser parser) "Debug"

stringToInt :: String -> Maybe Int
stringToInt s = read <$> parseMaybe parser s
  where parser = many digitChar :: Parser String

stringToInt0 :: String -> Int
stringToInt0 = fromMaybe 0 . stringToInt

inputNotFound :: IOError -> IO String
inputNotFound _ = pure ""

readInput :: Int -> IO String
readInput day = readFile ("inputs/day" ++ show day ++ ".txt") `catch` inputNotFound
