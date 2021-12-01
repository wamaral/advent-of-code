module Day3Spec
  (spec)
  where

import           Day3
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day3part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day3part2 input `shouldBe` ""
