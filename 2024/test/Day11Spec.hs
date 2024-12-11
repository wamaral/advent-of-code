module Day11Spec
  (spec)
  where

import           Day11
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day11part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day11part2 input `shouldBe` ""
