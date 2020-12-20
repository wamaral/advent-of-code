module Day11Spec
  (spec)
  where

import           Day11
import           Test.Hspec

input :: String
input = unlines
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day11part1 input `shouldBe` "37"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day11part2 input `shouldBe` "26"
