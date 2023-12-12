module Day6Spec
  (spec)
  where

import           Day6
import           Test.Hspec

input :: String
input = unlines
  [ "Time:      7  15   30"
  , "Distance:  9  40  200"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day6part1 input `shouldBe` "288"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day6part2 input `shouldBe` ""
