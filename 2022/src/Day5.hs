{-# LANGUAGE RecordWildCards #-}

module Day5
  (day5part1, day5part2)
  where

import           Common
import           Data.Function
import qualified Data.IntMap.Strict   as M
import           Data.List
import           Data.Maybe
import qualified Data.Stack           as S
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type CrateLine = [Maybe Char]
type CrateStack = S.Stack Char
type Crates = M.IntMap CrateStack

data Command = Command { cnt :: Int, src :: Int, dst :: Int } deriving Show

crateParser :: Parser (Maybe Char)
crateParser = choice
  [ Just <$> (char '[' *> (upperChar <* char ']'))
  , Nothing <$ string "   "
  ]

crateLineParser :: Parser CrateLine
crateLineParser = some (crateParser <* optional (char ' '))

crateLinesToCrates :: [CrateLine] -> Crates
crateLinesToCrates input = transpose input
  & map (stackEm . reverse . catMaybes)
  & zip [1..]
  & M.fromDistinctAscList
  where stackEm = foldl' S.stackPush S.stackNew

commandParser :: Parser Command
commandParser = do
  _ <- string "move "
  cnt <- intParser
  _ <- string " from "
  src <- intParser
  _ <- string " to "
  dst <- intParser
  _ <- newline
  pure Command{..}

inputParser :: Parser (Crates, [Command])
inputParser = do
  ls <- crateLinesToCrates <$> some (crateLineParser <* newline)
  _ <- some (digitChar <|> char ' ') <* newline
  _ <- newline
  cm <- someTill (commandParser <* optional newline) eof
  pure (ls, cm)

parseInput :: String -> (Crates, [Command])
parseInput = fromMaybe (M.empty, []) . parseMaybe inputParser

popN :: Int -> CrateStack -> (CrateStack, String)
popN n crates = atDef (crates, "") (iterate safePop (crates, "")) n
    where safePop (crate, acc) = case S.stackPop crate of
            Just (newCrate, popped) -> (newCrate, acc ++ [popped])
            Nothing                 -> (crate, acc)

moveBatchTransform :: (String -> String) -> Int -> Int -> Int -> Crates -> Crates
moveBatchTransform f from to n crates = do
  let fromCrate = (M.!) crates from
  let toCrate = (M.!) crates to
  let (newFrom, popped) = popN n fromCrate
  let newTo = foldl' S.stackPush toCrate (f popped)
  M.insert to newTo $ M.insert from newFrom crates

run :: (String -> String) -> Crates -> [Command] -> String
run f crates commands = foldl' (\c (Command n from to) -> moveBatchTransform f from to n c) crates commands
  & M.elems
  & mapM S.stackPeek
  & fromMaybe ""

day5part1 :: String -> String
day5part1 = uncurry (run id) . parseInput

day5part2 :: String -> String
day5part2 = uncurry (run reverse) . parseInput
