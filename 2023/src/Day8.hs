{-# LANGUAGE RecordWildCards #-}

module Day8
  (day8part1, day8part2)
  where

import Common
import Data.Function
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data Instruction = L | R deriving (Eq, Show)
type Position = String
type PositionMap = M.Map Position (Position, Position)
data Input = Input { instr :: [Instruction], pos :: PositionMap } deriving Show
data MapState = MapState { instructions :: [Instruction], positions :: PositionMap, currentPosition :: Position, steps :: Int }

instance Show MapState where
  show s =  "@" ++ currentPosition s ++ " #" ++ show (steps s) ++ " :: " ++ show (take 10 $ instructions s)

instructionParser :: Parser Instruction
instructionParser = choice
  [ L <$ char 'L'
  , R <$ char 'R'
  ]

positionParser :: Parser (Position, (Position, Position))
positionParser = do
  src <- count 3 upperChar
  _ <- string " = ("
  l <- count 3 upperChar
  _ <- string ", "
  r <- count 3 upperChar
  _ <- string ")"
  pure (src, (l, r))

inputParser :: Parser Input
inputParser = do
  instr <- many instructionParser
  _ <- newline
  _ <- newline
  pos <- M.fromList <$> many (positionParser <* optional newline)
  pure Input{..}

parseInput :: String -> Input
parseInput = fromMaybe Input{instr = [], pos = M.empty} . parseMaybe inputParser

startingState :: Position -> Input -> MapState
startingState startingPos input = MapState
  { instructions = cycle (instr input)
  , positions   = pos input
  , currentPosition = startingPos
  , steps = 0
  }

walk :: MapState -> MapState
walk state = do
  let instruction = head $ instructions state
  let (l, r) = (M.!) (positions state) (currentPosition state)
  let newPos = if instruction == L then l else r
  MapState
    { instructions = tail $ instructions state
    , positions = positions state
    , currentPosition = newPos
    , steps = succ $ steps state
    }

runUntilPosition :: (Position -> Bool) -> MapState -> MapState
runUntilPosition f = head . dropWhile (f . currentPosition) . iterate walk

day8part1 :: String -> String
day8part1 input = parseInput input
  & startingState "AAA"
  & runUntilPosition (/= "ZZZ")
  & steps
  & show

day8part2 :: String -> String
day8part2 input = initials
  & map (steps . runUntilPosition (not . isSuffixOf "Z") . (`startingState` parsed))
  & foldl lcm 1
  & show
  where
    parsed = parseInput input
    initials = filter (isSuffixOf "A") $ M.keys (pos parsed)
