module Day9Spec
  (spec)
  where

import           Day9
import           Test.Hspec

input :: String
input = unlines
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day9part1 input `shouldBe` "13"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day9part2 input `shouldBe` ""
