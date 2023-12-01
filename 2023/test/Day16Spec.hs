module Day16Spec
  (spec)
  where

import           Day16
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day16part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day16part2 input `shouldBe` ""
