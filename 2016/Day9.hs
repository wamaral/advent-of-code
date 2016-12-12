module Day9 where

import           Text.Megaparsec        hiding (count)
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.String

data Marker = Marker { len   :: Int
                     , times :: Int
                     } deriving Show

-- Parser
intVal :: Parser Int
intVal = read <$> some digitChar

marker :: Parser Marker
marker = between (char '(') (char ')') $ Marker <$> intVal <*> (char 'x' *> intVal)

subsequent :: Marker -> Parser String
subsequent (Marker ln _) = P.count ln anyChar

strParser :: Parser String
strParser = many (noneOf "(")

markerParser :: Parser String
markerParser = do
  mark <- marker
  concat . replicate (times mark) <$> subsequent mark

fullParser :: Parser String
fullParser = concat <$> someTill (markerParser <|> strParser) eof

decode :: String -> String
decode = concat . parse fullParser ""

-- Main
main :: IO ()
main = do
  input <- readFile "input/9"
  let decoded = decode input
  putStr "1. "
  print $ length decoded
  putStr "2. "
  putStrLn "TODO"
