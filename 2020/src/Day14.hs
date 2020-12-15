module Day14
  (day14part1, day14part2)
  where

import           Common
import           Data.Bits
import           Data.Function
import           Data.List
import qualified Data.Map             as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

data MaskBit = On | Off | Pass deriving (Eq, Show)
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
  & foldl' updateBit val
  where
    updateBit :: Value -> (Int, MaskBit) -> Value
    updateBit v (i, On)   = setBit v i
    updateBit v (i, Off)  = clearBit v i
    updateBit v (_, Pass) = v

addressToMask :: Address -> Mask
addressToMask addr = reverse $ map (\i -> if testBit addr i then On else Off) [0..35]

mergeMaskBit :: MaskBit -> MaskBit -> MaskBit
mergeMaskBit b1 Off = b1
mergeMaskBit _ b2   = b2

allCombinations :: Mask -> Mask -> [Address]
allCombinations m1 m2 = foldl' duplicateBits [addr1] [0..35]
  where
    addr1 = maskValue 0 m1
    addr2 = maskValue 0 m2
    duplicateBits addrs i = addrs ++ if testBit addr1 i == testBit addr2 i then [] else map (`complementBit` i) addrs

maskAddresses :: Address -> Mask -> [Address]
maskAddresses addr m = addressToMask addr
  & zipWith (flip mergeMaskBit) m
  & (\x -> (turn On x, turn Off x))
  & uncurry allCombinations
  where
    turn b = map (\x -> if x == Pass then b else x)

runInstruction :: Program -> Instruction -> Program
runInstruction prog (SetMask m) = prog {mask = m}
runInstruction prog (SetMem addr v) = prog {memory = newMemory}
  where newMemory = M.insert addr (maskValue v (mask prog)) (memory prog)

runInstructionV2 :: Program -> Instruction -> Program
runInstructionV2 prog (SetMask m) = prog {mask = m}
runInstructionV2 prog (SetMem addr v) = prog {memory = newMemory}
  where newMemory = foldl' (\m a -> M.insert a v m) (memory prog) (maskAddresses addr (mask prog))

runProgram :: (Program -> Instruction -> Program) -> String -> String
runProgram p = show . sum . M.elems . memory . foldl' p newProgram . readListOf instructionParser

day14part1 :: String -> String
day14part1 = runProgram runInstruction

day14part2 :: String -> String
day14part2 = runProgram runInstructionV2
