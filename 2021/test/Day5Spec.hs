module Day5Spec
  (spec)
  where

import           Day5
import           Test.Hspec

input :: String
input = unlines
  [ "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day5part1 input `shouldBe` "5"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day5part2 input `shouldBe` ""
