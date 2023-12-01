module Day5Spec
  (spec)
  where

import           Day5
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day5part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day5part2 input `shouldBe` ""
