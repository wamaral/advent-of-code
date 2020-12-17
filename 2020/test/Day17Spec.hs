module Day17Spec
  (spec)
  where

import           Day17
import           Test.Hspec

input :: String
input = unlines
  [ ".#."
  , "..#"
  , "###"
  ]


spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day17part1 input `shouldBe` "112"
  describe "part 2" $ do
    it "runs provided example" $ do
      day17part2 input `shouldBe` "848"
