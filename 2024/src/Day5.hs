module Day5
  (day5part1, day5part2)
  where

import           Common
import           Data.Bifunctor
import           Data.Either
import           Data.Function
import qualified Data.IntMap.Strict as M
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

type OrderRule = (Int, Int)
type PageList = [Int]
type Input = ([OrderRule], [PageList])
type OrderMap = M.IntMap (S.Set Int)

ruleParser :: Parser OrderRule
ruleParser = do
  a <- intParser
  _ <- char '|'
  b <- intParser
  return (a,b)

pageListParser :: Parser PageList
pageListParser = intParser `sepBy` char ','

inputParser :: Parser Input
inputParser = do
  rules <- manyTill (ruleParser <* newline) newline
  pages <- manyTill (pageListParser <* newline) eof
  return (rules, pages)

parseInput :: String -> Input
parseInput = fromRight ([],[]) . parse inputParser "Day5"

toOrderMap :: Input -> OrderMap
toOrderMap (rules,_) = M.fromListWith S.union $ map (second S.singleton) rules

isWrongAfter :: OrderMap -> [Int] -> Bool
isWrongAfter _ [] = False
isWrongAfter orderMap pages = any (`S.member` goodAfters) befores
  where
    goodAfters = M.findWithDefault S.empty page orderMap
    (befores, page) = (init pages, last pages)

allGood :: OrderMap -> [Int] -> Bool
allGood orderMap pages = inits pages & tail & (not . any (isWrongAfter orderMap))

orderPages :: OrderMap -> [Int] -> S.Set Int -> [Int]
orderPages orderMap ordered remaining = if S.null remaining
  then ordered
  else orderPages orderMap (ordered ++ [next]) (remainingWithout next)
  where
    getP p = M.findWithDefault S.empty p orderMap
    remainingWithout p = S.delete p remaining
    containsAll p = remainingWithout p `S.isSubsetOf` getP p
    next = filter containsAll (S.toList remaining) & head

midPoint :: [Int] -> Int
midPoint xs = xs !! (length xs `div` 2)

day5part1 :: String -> String
day5part1 input = pages
  & filter (allGood orderMap)
  & map midPoint
  & sum
  & show
  where
    parsedInput = parseInput input
    orderMap = toOrderMap parsedInput
    pages = snd parsedInput

day5part2 :: String -> String
day5part2 input = pages
  & filter (not . allGood orderMap)
  & map S.fromList
  & map (orderPages orderMap [])
  & map midPoint
  & sum
  & show
  where
    parsedInput = parseInput input
    orderMap = toOrderMap parsedInput
    pages = snd parsedInput
