module Day14Spec
  (spec)
  where

import           Day14
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day14part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day14part2 input `shouldBe` ""
