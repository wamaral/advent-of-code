module Day7Spec
  (spec)
  where

import           Day7
import           Test.Hspec

input :: String
input = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day7part1 input `shouldBe` "37"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day7part2 input `shouldBe` ""
