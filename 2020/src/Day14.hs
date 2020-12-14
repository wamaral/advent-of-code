module Day14
  (day14part1, day14part2)
  where

import           Common
import           Data.Bits
import           Data.Function
import qualified Data.Map             as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

data MaskBit = On | Off | Pass deriving Show
type Mask = [MaskBit]
type Address = Int
type Value = Int
type Memory = M.Map Address Value

data Instruction = SetMask Mask | SetMem Address Value deriving Show

data Program = Program
  { mask   :: Mask
  , memory :: Memory
  } deriving Show

maskBitParser :: Parser MaskBit
maskBitParser = choice
  [ On <$ char '1'
  , Off <$ char '0'
  , Pass <$ char 'X'
  ]

instructionParser :: Parser Instruction
instructionParser = choice
  [ try $ SetMask <$> (string "mask = " *> many maskBitParser)
  , try $ SetMem <$> (string "mem[" *> intParser) <*> (string "] = " *> intParser)
  ]

newProgram :: Program
newProgram = Program {mask = replicate 36 Pass, memory = M.empty}

maskValue :: Value -> Mask -> Value
maskValue val m = reverse m
  & zip [0..]
  & foldl updateBit val
  where
    updateBit :: Value -> (Int, MaskBit) -> Value
    updateBit v (i, On)   = setBit v i
    updateBit v (i, Off)  = clearBit v i
    updateBit v (_, Pass) = v

runInstruction :: Program -> Instruction -> Program
runInstruction prog (SetMask m) = prog {mask = m}
runInstruction prog (SetMem addr v) = prog {memory = M.insert addr (maskValue v (mask prog)) (memory prog)}

day14part1 :: String -> String
day14part1 = show . sum . M.elems . memory . foldl runInstruction newProgram . readListOf instructionParser

day14part2 :: String -> String
day14part2 _ = ""
