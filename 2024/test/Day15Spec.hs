module Day15Spec
  (spec)
  where

import           Day15
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day15part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day15part2 input `shouldBe` ""
