module Day12Spec
  (spec)
  where

import           Day12
import           Test.Hspec

input :: String
input = unlines
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day12part1 input `shouldBe` "31"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day12part2 input `shouldBe` "29"
