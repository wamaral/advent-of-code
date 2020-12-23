{-# LANGUAGE RecordWildCards #-}

module Day21
  (day21part1, day21part2)
  where

import           Common
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set             as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Ingredient = String
type Allergen = String
data Food = Food
  { ingredients :: [Ingredient]
  , allergens   :: [Allergen]
  } deriving Show

data Matcher = Matcher
  { mingredient :: Ingredient
  , mallergens  :: [Allergen]
  , mallergen   :: Maybe Allergen
  } deriving Show

foodParser :: Parser Food
foodParser = do
  ingredients <- filter (not . null) <$> many letterChar `sepEndBy` char ' '
  _ <- string "(contains "
  allergens <- many letterChar `sepBy` string ", "
  _ <- char ')'
  return Food{..}

allIngredients :: [Food] -> [Ingredient]
allIngredients = concatMap ingredients

uniqueIngredients :: [Food] -> S.Set Ingredient
uniqueIngredients = S.fromList . allIngredients

allAllergens :: [Food] -> [Allergen]
allAllergens = concatMap allergens

uniqueAllergens :: [Food] -> S.Set Allergen
uniqueAllergens = S.fromList . allAllergens

foodToMatchers :: [Food] -> [Matcher]
foodToMatchers menu = uniqueIngredients menu
  & S.toList
  & map (\i -> Matcher {mingredient = i, mallergens = possibleAllergensForIngredient menu i, mallergen = Nothing})

possibleAllergensForIngredient :: [Food] -> Ingredient -> [Allergen]
possibleAllergensForIngredient menu ingredient = filter (elem ingredient . ingredients) menu & uniqueAllergens & S.toList

alwaysIngredientWhenAllergen :: [Food] -> Allergen -> Ingredient -> Bool
alwaysIngredientWhenAllergen menu allergen ingredient = filter (elem allergen . allergens) menu & all (elem ingredient . ingredients)

shrink :: [Food] -> [Matcher] -> [Allergen] -> [Matcher]
shrink _ unmatched [] = unmatched
shrink menu matchers allergens = matched ++ shrink menu unmatched remainingAllergens
  where
    possibleAllergens matcher = mallergens matcher `intersect` allergens & filter (\a -> alwaysIngredientWhenAllergen menu a (mingredient matcher))
    isMatch matcher = length (possibleAllergens matcher) == 1
    updateMatch matcher = matcher {mallergen = if isMatch matcher then Just (head $ possibleAllergens matcher) else Nothing}
    applied = map updateMatch matchers
    (matched, unmatched) = partition (isJust . mallergen) applied
    remainingAllergens = allergens \\ mapMaybe mallergen matched

day21part1 :: String -> String
day21part1 input = menu
  & foodToMatchers
  & (\ms -> shrink menu ms menuAllergens)
  & filter (isNothing . mallergen)
  & map mingredient
  & map countAppearances
  & sum
  & show
  where
    menu = readListOf foodParser input
    menuAllergens = uniqueAllergens menu & S.toList
    countAppearances ingredient = filter (== ingredient) (allIngredients menu) & length

day21part2 :: String -> String
day21part2 input = menu
  & foodToMatchers
  & (\ms -> shrink menu ms menuAllergens)
  & filter (isJust . mallergen)
  & sortOn mallergen
  & map mingredient
  & intercalate ","
  where
    menu = readListOf foodParser input
    menuAllergens = uniqueAllergens menu & S.toList
