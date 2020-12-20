module Day6Spec
  (spec)
  where

import           Day6
import           Test.Hspec

input :: String
input = unlines
  [ "abcx"
  , "abcy"
  , "abcz"
  ]

input2 :: String
input2 = unlines
  [ "abc"
  , ""
  , "a"
  , "b"
  , "c"
  , ""
  , "ab"
  , "ac"
  , ""
  , "a"
  , "a"
  , "a"
  , "a"
  , ""
  , "b"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day6part1 input `shouldBe` "6"
      day6part1 input2 `shouldBe` "11"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day6part2 input2 `shouldBe` "6"
