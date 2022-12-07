module Day5Spec
  (spec)
  where

import           Day5
import           Test.Hspec

input :: String
input = unlines
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day5part1 input `shouldBe` "CMZ"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day5part2 input `shouldBe` "MCD"
