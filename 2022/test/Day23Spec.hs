module Day23Spec
  (spec)
  where

import           Day23
import           Test.Hspec

input :: String
input = ""

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day23part1 input `shouldBe` ""
  describe "part 2" $ do
    it "runs provided examples" $ do
      day23part2 input `shouldBe` ""
