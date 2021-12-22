module Day21Spec
  (spec)
  where

import           Day21
import           Test.Hspec

input :: String
input = unlines
  [ "Player 1 starting position: 4"
  , "Player 2 starting position: 8"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day21part1 input `shouldBe` "739785"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day21part2 input `shouldBe` ""
