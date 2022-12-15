{-# LANGUAGE RecordWildCards #-}

module Day11
  (day11part1, day11part2)
  where

import           Common
import           Data.Function
import qualified Data.IntMap.Strict   as M
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Operation = Mult Int | Add Int | Square deriving Show

data Monkey = Monkey
  { monkeyId     :: Int
  , items        :: [Int]
  , operation    :: Operation
  , divisibleBy  :: Int
  , testTrue     :: Int
  , testFalse    :: Int
  , inspectCount :: Int
  } deriving Show

type Monkeys = M.IntMap Monkey

monkeyParser :: Parser Monkey
monkeyParser = do
  monkeyId <- string "Monkey " *> intParser <* string ":"
  _ <- newline
  _ <- string "  Starting items: "
  items <- intParser `sepBy` string ", "
  _ <- newline
  _ <- string "  Operation: new = old "
  operation <- operationParser
  _ <- newline
  _ <- string "  Test: divisible by "
  divisibleBy <- intParser
  _ <- newline
  _ <- string "    If true: throw to monkey "
  testTrue <- intParser
  _ <- newline
  _ <- string "    If false: throw to monkey "
  testFalse <- intParser
  _ <- newline
  let inspectCount = 0
  return Monkey{..}

operationParser :: Parser Operation
operationParser = choice
  [ Add <$> (string "+ " *> intParser)
  , Mult <$> try (string "* " *> intParser)
  , Square <$ string "* old"
  ]

inputParser :: Parser [Monkey]
inputParser = monkeyParser `sepBy` newline

parseInput :: String -> Monkeys
parseInput = M.fromDistinctAscList . maybe [] (map (\m -> (monkeyId m, m))) . parseMaybe inputParser

operate :: Operation -> Int -> Int
operate (Mult x) i = i * x
operate (Add x) i  = i + x
operate Square i   = i * i

monkeyTurn :: Monkeys -> Int -> Monkeys
monkeyTurn monkeys mId = do
  let monkey = (M.!) monkeys mId
  let monkeyTrue = (M.!) monkeys (testTrue monkey)
  let monkeyFalse = (M.!) monkeys (testFalse monkey)
  let worries = map (operate (operation monkey)) (items monkey)
  let bored = map (`div` 3) worries
  let (toMonkeyTrue, toMonkeyFalse) = partition (\x -> x `mod` divisibleBy monkey == 0) bored
  let monkey' = monkey { items = [], inspectCount = inspectCount monkey + length bored }
  let monkeyTrue' = monkeyTrue { items = items monkeyTrue ++ toMonkeyTrue }
  let monkeyFalse' = monkeyFalse { items = items monkeyFalse ++ toMonkeyFalse }
  let newMonkeys = M.fromList $ map (\m -> (monkeyId m, m)) [monkey', monkeyTrue', monkeyFalse']
  M.union newMonkeys monkeys

monkeyRound :: Monkeys -> Monkeys
monkeyRound monkeys = foldl' monkeyTurn monkeys [0..(pred $ M.size monkeys)]

day11part1 :: String -> String
day11part1 input = parseInput input
  & iterate monkeyRound
  & take 21
  & last
  & M.map inspectCount
  & M.elems
  & sort
  & reverse
  & take 2
  & product
  & show

day11part2 :: String -> String
day11part2 _ = ""
