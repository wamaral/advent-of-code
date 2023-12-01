module Day2Spec
  (spec)
  where

import           Day2
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day2part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day2part2 input `shouldBe` ""
