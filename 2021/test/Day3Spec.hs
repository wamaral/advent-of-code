module Day3Spec
  (spec)
  where

import           Day3
import           Test.Hspec

input :: String
input = unlines
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day3part1 input `shouldBe` "198"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day3part2 input `shouldBe` ""
