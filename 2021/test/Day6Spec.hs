module Day6Spec
  (spec)
  where

import           Day6
import           Test.Hspec

input :: String
input = "3,4,3,1,2"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day6part1 input `shouldBe` "5934"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day6part2 input `shouldBe` ""
