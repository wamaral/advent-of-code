module Common where

import           Control.Exception
import           Data.Function
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Void
import           Debug.HTrace
import           Linear.V2
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

zipTail :: [a] -> [(a,a)]
zipTail xs = zip xs (tail xs)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = if x == x' then x else fixpoint f x'
  where x' = f x

listOfListsToV2Map :: [[a]] -> M.Map (V2 Int) a
listOfListsToV2Map xs = zip [0..] xs
  & concatMap (\(i, line) -> map (\(j, x) -> (V2 i j, x)) $ zip [0..] line)
  & M.fromList

-- | trace but only after something has evaluated to WHNF
trace' :: String -> a -> a
trace' str x = htrace (x `seq` str) x

traceWith :: (a -> String) -> a -> a
traceWith f v = htrace (f v) v

traceShowWith :: Show a => (String -> String) -> a -> a
traceShowWith f v = htrace (f (show v)) v

traceShowLabel :: Show a => String -> a -> a
traceShowLabel s = traceShowWith ((s ++ " => ") ++)

traceShowId' :: Show a => a -> a
traceShowId' x = htrace (show x) x

inputNotFound :: IOError -> IO String
inputNotFound _ = pure ""

readInput :: Int -> IO String
readInput day = readFile ("inputs/day" ++ show day ++ ".txt") `catch` inputNotFound
