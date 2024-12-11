module Day9Spec
  (spec)
  where

import           Day9
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day9part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day9part2 input `shouldBe` ""
