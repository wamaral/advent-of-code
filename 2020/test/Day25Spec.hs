module Day25Spec
  (spec)
  where

import           Day25
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day25part1 "" `shouldBe` ""
  describe "part 2" $ do
    it "runs provided example" $ do
      day25part2 "" `shouldBe` ""
