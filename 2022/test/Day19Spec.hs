module Day19Spec
  (spec)
  where

import           Day19
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day19part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day19part2 input `shouldBe` ""
