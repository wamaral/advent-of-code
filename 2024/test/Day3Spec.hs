module Day3Spec
  (spec)
  where

import           Day3
import           Test.Hspec

input :: String
input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

input2 :: String
input2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day3part1 input `shouldBe` "161"
      day3part1 input2 `shouldBe` "161"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day3part2 input `shouldBe` "161"
      day3part2 input2 `shouldBe` "48"
