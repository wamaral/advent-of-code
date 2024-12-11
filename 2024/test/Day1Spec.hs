module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "3   4"
  , "4   3"
  , "2   5"
  , "1   3"
  , "3   9"
  , "3   3"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "11"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` "31"
