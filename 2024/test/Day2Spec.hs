module Day2Spec
  (spec)
  where

import           Day2
import           Test.Hspec

input :: String
input = unlines
  [ "7 6 4 2 1"
  , "1 2 7 8 9"
  , "9 7 6 2 1"
  , "1 3 2 4 5"
  , "8 6 4 4 1"
  , "1 3 6 7 9"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day2part1 input `shouldBe` "2"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day2part2 input `shouldBe` "4"
