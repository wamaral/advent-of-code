module Day5Spec
  (spec)
  where

import           Day5
import           Test.Hspec

input :: String
input = unlines
  [ "47|53"
  , "97|13"
  , "97|61"
  , "97|47"
  , "75|29"
  , "61|13"
  , "75|53"
  , "29|13"
  , "97|29"
  , "53|29"
  , "61|53"
  , "97|53"
  , "61|29"
  , "47|13"
  , "75|47"
  , "97|75"
  , "47|61"
  , "75|61"
  , "47|29"
  , "75|13"
  , "53|13"
  , ""
  , "75,47,61,53,29"
  , "97,61,53,29,13"
  , "75,29,13"
  , "75,97,47,61,53"
  , "61,13,29"
  , "97,13,75,29,47"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day5part1 input `shouldBe` "143"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day5part2 input `shouldBe` "123"
