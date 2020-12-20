module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "1721"
  , "979"
  , "366"
  , "299"
  , "675"
  , "1456"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "514579"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` "241861950"
