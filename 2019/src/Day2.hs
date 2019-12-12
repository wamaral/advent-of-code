module Day2
  (day2part1, day2part2)
  where

import           Common
import           Control.Lens
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- Questionable code follows

type Position = Int
type Block = [Int]
data Program = Running [Block] | Finished [Block] deriving Show

readInts :: String -> [Int]
readInts = fromMaybe [] . parseMaybe (intParser `sepEndBy` char ',' <* optional newline)

readProgram :: String -> Program
readProgram = Running . chunksOf 4 . readInts

positionInBlocks :: Position -> (Int, Int)
positionInBlocks x = quotRem x 4

getValueInPosition :: Position -> Program -> Int
getValueInPosition _ (Finished _) = 0
getValueInPosition x (Running program) = fromMaybe 0 $ program ^? ix a . ix b
  where (a, b) = positionInBlocks x

updateValueInPosition :: Position -> Int -> Program -> Program
updateValueInPosition _ _ p@(Finished _) = p
updateValueInPosition x y (Running program) = Running $ program & ix a . ix b %~ const y
  where (a, b) = positionInBlocks x

add :: Position -> Position -> Program -> Int
add p1 p2 program = getValueInPosition p1 program + getValueInPosition p2 program

multiply :: Position -> Position -> Program -> Int
multiply p1 p2 program = getValueInPosition p1 program * getValueInPosition p2 program

applyOperation :: Program -> Block -> Program
applyOperation p@(Finished _) _ = p
applyOperation p@(Running program) (a:b:c:d:_) = case a of
  1  -> updateValueInPosition d (add b c p) p
  2  -> updateValueInPosition d (multiply b c p) p
  99 -> Finished program
  _  -> Running program -- noop
applyOperation p@(Running _) _ = p

result :: Program -> Int
result (Finished p) = head . head $ p
result (Running _)  = 0

day2part1 :: String -> String
day2part1 input = do
  let program = updateValueInPosition 2 2 $ updateValueInPosition 1 12 $ readProgram input
  show $ result $ runProgram program
  where
    runProgram program@(Running blocks) = foldl applyOperation program blocks
    runProgram program@(Finished _)     = program

day2part2 :: String -> String
day2part2 input = do
  let baseProgram = readProgram input
  let programs = [(x, y, updateValueInPosition 1 x $ updateValueInPosition 2 y baseProgram) | x <- [0..100], y <- [0..100]]
  let evaluatedPrograms = map runProgram programs
  let found = find (\(_, _, program) -> result program == 19690720) evaluatedPrograms
  case found of
    Nothing        -> "Error: no match found"
    Just (x, y, _) -> show $ 100 * x + y
  where
    runProgram (x, y, program@(Running blocks)) = (x, y, foldl applyOperation program blocks)
    runProgram (x, y, program@(Finished _))     = (x, y, program)
