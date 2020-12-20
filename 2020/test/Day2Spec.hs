module Day2Spec
  (spec)
  where

import           Day2
import           Test.Hspec

input :: String
input = unlines
  [ "1-3 a: abcde"
  , "1-3 b: cdefg"
  , "2-9 c: ccccccccc"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day2part1 input `shouldBe` "2"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day2part2 input `shouldBe` "1"
