module Day2Spec
  (spec)
  where

import           Day2
import           Test.Hspec

input :: String
input = unlines
  [ "forward 5"
  , "down 5"
  , "forward 8"
  , "up 3"
  , "down 8"
  , "forward 2"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day2part1 input `shouldBe` "150"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day2part2 input `shouldBe` ""
