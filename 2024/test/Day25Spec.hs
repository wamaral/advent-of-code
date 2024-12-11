module Day25Spec
  (spec)
  where

import           Day25
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day25part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day25part2 input `shouldBe` ""
