module Day24Spec
  (spec)
  where

import           Day24
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day24part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day24part2 input `shouldBe` ""
