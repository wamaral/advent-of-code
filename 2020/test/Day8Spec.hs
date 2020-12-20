module Day8Spec
  (spec)
  where

import           Day8
import           Test.Hspec

input :: String
input = unlines
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day8part1 input `shouldBe` "5"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day8part2 input `shouldBe` "8"
