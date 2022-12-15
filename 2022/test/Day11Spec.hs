module Day11Spec
  (spec)
  where

import           Day11
import           Test.Hspec

input :: String
input = unlines
  [ "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  , "Monkey 2:"
  , "  Starting items: 79, 60, 97"
  , "  Operation: new = old * old"
  , "  Test: divisible by 13"
  , "    If true: throw to monkey 1"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 3:"
  , "  Starting items: 74"
  , "  Operation: new = old + 3"
  , "  Test: divisible by 17"
  , "    If true: throw to monkey 0"
  , "    If false: throw to monkey 1"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day11part1 input `shouldBe` "10605"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day11part2 input `shouldBe` "2713310158"
