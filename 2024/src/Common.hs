{-# LANGUAGE BangPatterns   #-}

module Common where

import           Control.Exception
import           Data.Function
import           Data.IORef
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Void
import           Linear.V2
import           System.IO.Unsafe
import           Text.Megaparsec
import qualified Text.Megaparsec      as P
import           Text.Megaparsec.Char

type Parser = Parsec Void String

-- try Text.Megaparsec.Char.Lexer.decimal
intParser :: Parser Int
intParser = stringToInt0 <$> some digitChar

intParser' :: Int -> Int -> Parser Int
intParser' minLen maxLen = stringToInt0 <$> count' minLen maxLen digitChar

-- try Text.Megaparsec.Char.Lexer.[signed decimal]
signedIntParser :: Parser Int
signedIntParser = do
  sign <- optional $ char '-' <|> char '+'
  int <- stringToInt0 <$> some digitChar
  return $ if sign == Just '-' then negate int else int

singleDigitParser :: Parser Int
singleDigitParser = stringToInt0 . (: []) <$> digitChar

skipUntilP :: Parser a -> Parser a
skipUntilP parser = P.try $ skipManyTill anySingle (P.try parser)

linesParser :: Parser a -> Parser [a]
linesParser parser = someTill (parser <* optional newline) eof

chunkParser :: Parser a -> Parser [[a]]
chunkParser parser = some (parser <* optional newline) `sepBy` newline

readListOf :: Parser a -> String -> [a]
readListOf parser = fromMaybe [] . parseMaybe (linesParser parser)

readListOfDebug :: Show a => Parser a -> String -> String
readListOfDebug parser = either errorBundlePretty show . parse (linesParser parser) "Debug"

readChunksOf :: Parser a -> String -> [[a]]
readChunksOf parser = fromMaybe [] . parseMaybe (chunkParser parser)

readChunksOfDebug :: Show a => Parser a -> String -> String
readChunksOfDebug parser = either errorBundlePretty show . parse (chunkParser parser) "Debug"

stringToInt :: String -> Maybe Int
stringToInt s = read <$> parseMaybe parser s
  where parser = many digitChar :: Parser String

stringToInt0 :: String -> Int
stringToInt0 = fromMaybe 0 . stringToInt

concatInts :: [Int] -> Int
concatInts = stringToInt0 . concatMap show

zipTail :: [a] -> [(a,a)]
zipTail xs = zip xs (tail xs)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = if x == x' then x else fixpoint f x'
  where x' = f x

listOfListsToV2Map :: [[a]] -> M.Map (V2 Int) a
listOfListsToV2Map as = zip [0..] as
  & concatMap (\(y, line) -> map (\(x, a) -> (V2 x y, a)) $ zip [0..] line)
  & M.fromList

interpolate :: (Enum a, Ord a) => a -> a -> [a]
interpolate a b
  | a < b = [a..b]
  | a > b = [a,(pred a)..b]
  | otherwise = []

-- From: https://hackage.haskell.org/package/htrace-0.1/docs/src/Debug-HTrace.html#htrace
-- | Trace "str" on a separate line, and increase indentation of subsequent
--   traces while x is being evaluated.
htrace :: String -> a -> a
htrace str x = unsafePerformIO $ do
  lvl <- readIORef level
  putStrLn (replicate (2*lvl) ' ' ++ str)
  writeIORef level (lvl+1)
  let !vx = x
  writeIORef level lvl
  return vx
  where level = unsafePerformIO $ newIORef 0
-- End: htrace

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
