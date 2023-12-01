module Day21Spec
  (spec)
  where

import           Day21
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day21part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day21part2 input `shouldBe` ""
