module Day20Spec
  (spec)
  where

import           Day20
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day20part1 "" `shouldBe` ""
  describe "part 2" $ do
    it "runs provided example" $ do
      day20part2 "" `shouldBe` ""
