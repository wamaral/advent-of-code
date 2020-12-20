module Day21Spec
  (spec)
  where

import           Day21
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day21part1 "" `shouldBe` ""
  describe "part 2" $ do
    it "runs provided example" $ do
      day21part2 "" `shouldBe` ""
