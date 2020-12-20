module Day9Spec
  (spec)
  where

import           Day9
import           Test.Hspec

input :: String
input = unlines
  [ "35"
  , "20"
  , "15"
  , "25"
  , "47"
  , "40"
  , "62"
  , "55"
  , "65"
  , "95"
  , "102"
  , "117"
  , "150"
  , "182"
  , "127"
  , "219"
  , "299"
  , "277"
  , "309"
  , "576"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day9part1test input `shouldBe` "127"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day9part2test input `shouldBe` "62"
