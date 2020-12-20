module Day10Spec
  (spec)
  where

import           Day10
import           Test.Hspec

input :: String
input = unlines
  [ "16"
  , "10"
  , "15"
  , "5"
  , "1"
  , "11"
  , "7"
  , "19"
  , "6"
  , "12"
  , "4"
  ]

input2 :: String
input2 = unlines
  [ "28"
  , "33"
  , "18"
  , "42"
  , "31"
  , "14"
  , "46"
  , "20"
  , "48"
  , "47"
  , "24"
  , "23"
  , "49"
  , "45"
  , "19"
  , "38"
  , "39"
  , "11"
  , "1"
  , "32"
  , "25"
  , "35"
  , "8"
  , "17"
  , "7"
  , "9"
  , "4"
  , "2"
  , "34"
  , "10"
  , "3"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day10part1 input `shouldBe` "35"
      day10part1 input2 `shouldBe` "220"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day10part2 input `shouldBe` "8"
      day10part2 input2 `shouldBe` "19208"
