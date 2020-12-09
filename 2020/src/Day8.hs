module Day8
  (day8part1, day8part2)
  where

import           Common
import           Control.Monad.State
import qualified Data.Set             as S
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

data Op = Acc Int | Jmp Int | Nop Int deriving Show

data Machine = Machine
  { accumulator  :: Int
  , pointer      :: Int
  , operations   :: [Op]
  , visitedState :: S.Set Int
  , looped       :: Bool
  } deriving Show

opParser :: Parser Op
opParser = choice
  [ Acc <$> (string "acc " *> signedIntParser)
  , Jmp <$> (string "jmp " *> signedIntParser)
  , Nop <$> (string "nop " *> signedIntParser)
  ]

buildMachine :: [Op] -> Machine
buildMachine ops = Machine
  { accumulator = 0
  , pointer = 0
  , operations = ops
  , visitedState = S.fromList [0]
  , looped = False
  }

runMachine :: State Machine ()
runMachine = do
  machine <- get
  if looped machine then return () else do
    let currentOp = operations machine !! pointer machine
    let newAcc = case currentOp of
          Acc x -> accumulator machine + x
          _     -> accumulator machine
    let newPointer = case currentOp of
          Jmp x -> pointer machine + x
          _     -> succ $ pointer machine
    let newLooped = S.member newPointer (visitedState machine)
    let newVisitedState = S.insert newPointer (visitedState machine)
    put machine{accumulator = newAcc, pointer = newPointer, looped = newLooped, visitedState = newVisitedState}
    runMachine

day8part1 :: String -> String
day8part1 = show . accumulator . execState runMachine . buildMachine . readListOf opParser

day8part2 :: String -> String
day8part2 _ = ""
