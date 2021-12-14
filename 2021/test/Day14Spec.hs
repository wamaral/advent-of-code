module Day14Spec
  (spec)
  where

import           Day14
import           Test.Hspec

input :: String
input = unlines
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day14part1 input `shouldBe` "1588"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day14part2 input `shouldBe` ""
