module Day16Spec
  (spec)
  where

import           Day16
import           Test.Hspec

input1 :: String
input1 = unlines
  [ "class: 1-3 or 5-7"
  , "row: 6-11 or 33-44"
  , "seat: 13-40 or 45-50"
  , ""
  , "your ticket:"
  , "7,1,14"
  , ""
  , "nearby tickets:"
  , "7,3,47"
  , "40,4,50"
  , "55,2,20"
  , "38,6,12"
  ]

input2 :: String
input2 = unlines
  [ "departure class: 0-1 or 4-19"
  , "departure row: 0-5 or 8-19"
  , "seat: 0-13 or 16-19"
  , ""
  , "your ticket:"
  , "11,12,13"
  , ""
  , "nearby tickets:"
  , "3,9,18"
  , "15,1,5"
  , "5,14,9"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day16part1 input1 `shouldBe` "71"
  describe "part 2" $ do
    it "runs provided example" $ do
      day16part2 input2 `shouldBe` "132"
