module Day11Spec
  (spec)
  where

import           Day11
import           Test.Hspec

input :: String
input = unlines
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day11part1 input `shouldBe` "1656"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day11part2 input `shouldBe` "195"
