module Day6Spec
  (spec)
  where

import           Day6
import           Test.Hspec

input :: String
input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
input2 :: String
input2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
input3 :: String
input3 = "nppdvjthqldpwncqszvftbrmjlhg"
input4 :: String
input4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
input5 :: String
input5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day6part1 input `shouldBe` "7"
      day6part1 input2 `shouldBe` "5"
      day6part1 input3 `shouldBe` "6"
      day6part1 input4 `shouldBe` "10"
      day6part1 input5 `shouldBe` "11"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day6part2 input `shouldBe` "19"
      day6part2 input2 `shouldBe` "23"
      day6part2 input3 `shouldBe` "23"
      day6part2 input4 `shouldBe` "29"
      day6part2 input5 `shouldBe` "26"
