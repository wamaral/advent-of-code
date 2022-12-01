module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` ""
