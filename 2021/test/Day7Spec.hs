module Day7Spec
  (spec)
  where

import           Day7
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day7part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day7part2 input `shouldBe` ""
