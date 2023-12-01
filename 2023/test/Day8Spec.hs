module Day8Spec
  (spec)
  where

import           Day8
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day8part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day8part2 input `shouldBe` ""
