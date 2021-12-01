module Day22Spec
  (spec)
  where

import           Day22
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day22part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day22part2 input `shouldBe` ""
