module Day4Spec
  (spec)
  where

import           Day4
import           Test.Hspec

input :: String
input = unlines
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day4part1 input `shouldBe` "2"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day4part2 input `shouldBe` "4"
