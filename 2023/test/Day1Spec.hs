module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "1abc2"
  , "pqr3stu8vwx"
  , "a1b2c3d4e5f"
  , "treb7uchet"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "142"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` ""
