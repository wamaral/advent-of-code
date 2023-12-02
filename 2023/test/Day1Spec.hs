module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "1abc2"
  , "pqr3stu8vwx"
  , "a1b2c3d4e5f"
  , "treb7uchet"
  ]

input2 :: String
input2 = unlines
  [ "two1nine"
  , "eightwothree"
  , "abcone2threexyz"
  , "xtwone3four"
  , "4nineeightseven2"
  , "zoneight234"
  , "7pqrstsixteen"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "142"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` "142"
      day1part2 input2 `shouldBe` "281"
