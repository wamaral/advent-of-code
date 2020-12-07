{-# LANGUAGE RecordWildCards #-}

module Day7
  (day7part1, day7part2)
  where

import           Common
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char

type BagMap = M.Map String Bag

data Bag = Bag
  { color    :: String
  , children :: M.Map String Int
  } deriving Show

instance Eq Bag where
  a == b = color a == color b

childParser :: Parser (String, Int)
childParser = do
  _ <- optional spaceChar
  childCount <- intParser
  _ <- spaceChar
  childColor <- unwords <$> count 2 (many lowerChar <* spaceChar)
  _ <- string "bag"
  _ <- optional $ char 's'
  _ <- optional $ char ','
  return (childColor, childCount)

bagParser :: Parser Bag
bagParser = do
  color <- unwords <$> count 2 (many lowerChar <* spaceChar)
  _ <- string "bags contain "
  _ <- try $ optional $ string "no other bags"
  children <- M.fromList <$> many childParser
  _ <- char '.'
  return Bag {..}

hasChild :: String -> Bag -> Bool
hasChild child bag = M.member child (children bag)

hasChildRec :: String -> [Bag] -> [Bag]
hasChildRec _ [] = []
hasChildRec child bags = myParents ++ concatMap (\x -> hasChildRec (color x) bags) myParents
  where myParents = filter (hasChild child) bags

getChildCountRec :: String -> BagMap -> Int
getChildCountRec bagColor bagmap = succ $ sum $ map (\childName -> howManyOfChild childName * getChildCountRec childName bagmap) myChildrenNames
  where
    myself = fromMaybe Bag{color = bagColor, children = M.empty} $ M.lookup bagColor bagmap
    myChildrenNames = M.keys $ children myself
    howManyOfChild childName = fromMaybe 0 $ M.lookup childName (children myself)

toBagMap :: [Bag] -> BagMap
toBagMap bags = M.fromList $ map (\x -> (color x, x)) bags

day7part1 :: String -> String
day7part1 = show . length . nub . hasChildRec "shiny gold" . readListOf bagParser

day7part2 :: String -> String
day7part2 = show . pred . getChildCountRec "shiny gold" . toBagMap . readListOf bagParser
