module Day18Spec
  (spec)
  where

import           Day18
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day18part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day18part2 input `shouldBe` ""
