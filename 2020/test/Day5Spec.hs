module Day5Spec
  (spec)
  where

import           Day5
import           Test.Hspec

input :: String
input = unlines
  [ "FBFBBFFRLR"
  , "BFFFBBFRRR"
  , "FFFBBBFRRR"
  , "BBFFBBFRLL"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day5part1 input `shouldBe` "820"
