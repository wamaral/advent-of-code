module Day10Spec
  (spec)
  where

import           Day10
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day10part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day10part2 input `shouldBe` ""
