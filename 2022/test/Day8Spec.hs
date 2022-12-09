module Day8Spec
  (spec)
  where

import           Day8
import           Test.Hspec

input :: String
input = unlines
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day8part1 input `shouldBe` "21"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day8part2 input `shouldBe` "8"
