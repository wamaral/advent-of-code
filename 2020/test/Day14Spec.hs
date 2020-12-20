module Day14Spec
  (spec)
  where

import           Day14
import           Test.Hspec

input :: String
input = unlines
  [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  , "mem[8] = 11"
  , "mem[7] = 101"
  , "mem[8] = 0"
  ]

input2 :: String
input2 = unlines
  [ "mask = 000000000000000000000000000000X1001X"
  , "mem[42] = 100"
  , "mask = 00000000000000000000000000000000X0XX"
  , "mem[26] = 1"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day14part1 input `shouldBe` "165"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day14part2 input2 `shouldBe` "208"
