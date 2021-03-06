module Day23Spec
  (spec)
  where

import           Day23
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day23part1 "389125467" `shouldBe` "67384529"
  describe "part 2" $ do
    it "runs provided example" $ do
      day23part2 "389125467" `shouldBe` "149245887792"
