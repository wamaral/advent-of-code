module Day13Spec
  (spec)
  where

import           Day13
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day13part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day13part2 input `shouldBe` ""
