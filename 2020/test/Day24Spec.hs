module Day24Spec
  (spec)
  where

import           Day24
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day24part1 "" `shouldBe` ""
  describe "part 2" $ do
    it "runs provided example" $ do
      day24part2 "" `shouldBe` ""
