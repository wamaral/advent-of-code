module Day10
  (day10part1, day10part2)
  where

import           Common
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Instruction = Noop | AddX Int deriving Show
data Cpu = Cpu { register :: Int, output :: [Int] } deriving Show

newCpu :: Cpu
newCpu = Cpu 1 []

instructionParser :: Parser Instruction
instructionParser = choice
  [ Noop <$ string "noop"
  , AddX <$> (string "addx " *> signedIntParser)
  ]

runInstruction :: Cpu -> Instruction -> Cpu
runInstruction (Cpu r o) Noop     = Cpu r (o ++ [r])
runInstruction (Cpu r o) (AddX x) = Cpu (r + x) (o ++ [r, r])

interestingSignals :: Cpu -> [Int]
interestingSignals (Cpu _ o) = map getSignal interestingCycles
  where
    getSignal s = s * (o !! pred s)
    interestingCycles = [20, 60, 100, 140, 180, 220]

day10part1 :: String -> String
day10part1 = show . sum . interestingSignals . foldl' runInstruction newCpu . readListOf instructionParser

day10part2 :: String -> String
day10part2 _ = ""
