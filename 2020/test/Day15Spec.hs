module Day15Spec
  (spec)
  where

import           Day15
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day15part1 "0,3,6" `shouldBe` "436"
      day15part1 "1,3,2" `shouldBe` "1"
      day15part1 "2,1,3" `shouldBe` "10"
      day15part1 "1,2,3" `shouldBe` "27"
      day15part1 "2,3,1" `shouldBe` "78"
      day15part1 "3,2,1" `shouldBe` "438"
      day15part1 "3,1,2" `shouldBe` "1836"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day15part2 "0,3,6" `shouldBe` "436"
      day15part2 "1,3,2" `shouldBe` "2578"
      day15part2 "2,1,3" `shouldBe` "3544142"
      day15part2 "1,2,3" `shouldBe` "261214"
      day15part2 "2,3,1" `shouldBe` "6895259"
      day15part2 "3,2,1" `shouldBe` "18"
      day15part2 "3,1,2" `shouldBe` "362"
