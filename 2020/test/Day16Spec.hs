module Day16Spec
  (spec)
  where

import           Day16
import           Test.Hspec

input :: String
input = unlines
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

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day16part1 input `shouldBe` "71"
