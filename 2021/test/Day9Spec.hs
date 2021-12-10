module Day9Spec
  (spec)
  where

import           Day9
import           Test.Hspec

input :: String
input = unlines
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day9part1 input `shouldBe` "15"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day9part2 input `shouldBe` ""
