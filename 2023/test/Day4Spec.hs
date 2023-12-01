module Day4Spec
  (spec)
  where

import           Day4
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day4part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day4part2 input `shouldBe` ""
