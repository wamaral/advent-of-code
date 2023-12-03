module Day3
  (day3part1, day3part2)
  where

import           Common
import           Data.Function
import           Data.List (nub)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Linear.V2
import           Text.Megaparsec
import           Text.Megaparsec.Char

data EnginePart = Digit Int | Symbol Char | Empty deriving Eq
type Position = V2 Int
type Engine = M.Map Position EnginePart

instance Show EnginePart where
  show (Digit i) = show i
  show (Symbol s) = show s
  show Empty = "Empty"

isDigit :: EnginePart -> Bool
isDigit (Digit _) = True
isDigit _ = False

isSymbol :: EnginePart -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

partParser :: Parser EnginePart
partParser = choice
  [ Digit <$> singleDigitParser
  , Empty <$ (char '.')
  , Symbol <$> printChar
  ]

engineParser :: Parser Engine
engineParser = listOfListsToV2Map <$> linesParser (some partParser)

parseInput :: String -> Engine
parseInput = fromMaybe M.empty . parseMaybe engineParser

neighbourhood :: [Position]
neighbourhood = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1),
                 V2 (-1) 0,    V2 0 0,    V2 1 0,
                 V2 (-1) 1,    V2 0 1,    V2 1 1]

neighbours :: Position -> [Position]
neighbours p = map (+ p) neighbourhood

neighboursWithDigits :: Engine -> Position -> [Position]
neighboursWithDigits engine p = neighbours p
  & filter (\pos -> maybe False isDigit $ M.lookup pos engine)

digitStart :: Engine -> Position -> Position
digitStart engine (V2 x y) = coords
  & takeWhile (\p -> maybe False isDigit $ M.lookup p engine)
  & reverse
  & head
  where
    xs = reverse [0..x]
    coords = map (`V2` y) xs

symbolPositions :: Engine -> [Position]
symbolPositions = M.keys . M.filter isSymbol

numberAt :: Engine -> Position -> Int
numberAt engine (V2 x y) = map (`V2` y) [x..]
  & takeWhile (\p -> maybe False isDigit $ M.lookup p engine)
  & map (\p -> M.lookup p engine)
  & catMaybes
  & concatMap show
  & stringToInt0

day3part1 :: String -> String
day3part1 input = engine
  & symbolPositions
  & concatMap (neighboursWithDigits engine)
  & map (digitStart engine)
  & nub
  & map (numberAt engine)
  & sum
  & show
  where engine = parseInput input

day3part2 :: String -> String
day3part2 _ = ""
