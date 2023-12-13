module Day7Spec
  (spec)
  where

import           Day7
import           Test.Hspec

input :: String
input = unlines
  [ "32T3K 765"
  , "T55J5 684"
  , "KK677 28"
  , "KTJJT 220"
  , "QQQJA 483"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day7part1 input `shouldBe` "6440"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day7part2 input `shouldBe` "5905"
