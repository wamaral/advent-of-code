module Day6Spec
  (spec)
  where

import           Day6
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day6part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day6part2 input `shouldBe` ""
