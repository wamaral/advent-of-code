module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "199"
  , "200"
  , "208"
  , "210"
  , "200"
  , "207"
  , "240"
  , "269"
  , "260"
  , "263"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "7"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` "5"
