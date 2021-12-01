module Day20Spec
  (spec)
  where

import           Day20
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day20part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day20part2 input `shouldBe` ""
