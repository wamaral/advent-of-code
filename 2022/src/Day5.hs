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

moveOne :: Int -> Int -> Crates -> Crates
moveOne from to crates = do
  let fromCrate = (M.!) crates from
  let toCrate = (M.!) crates to
  case S.stackPop fromCrate of
    Just (newFrom, popped) -> do
      let newTo = S.stackPush toCrate popped
      M.insert to newTo $ M.insert from newFrom crates
    Nothing -> crates

runCommand :: Crates -> Command -> Crates
runCommand crates (Command n from to) = atDef crates (iterate (moveOne from to) crates) n

day5part1 :: String -> String
day5part1 input = foldl' runCommand crates commands
  & M.elems
  & mapM S.stackPeek
  & fromMaybe ""
  where (crates, commands) = parseInput input

day5part2 :: String -> String
day5part2 _ = ""
