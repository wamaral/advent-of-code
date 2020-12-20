module Day22Spec
  (spec)
  where

import           Day22
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day22part1 "" `shouldBe` ""
  describe "part 2" $ do
    it "runs provided example" $ do
      day22part2 "" `shouldBe` ""
