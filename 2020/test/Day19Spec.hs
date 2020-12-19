module Day19Spec
  (spec)
  where

import           Day19
import           Test.Hspec

input1 :: String
input1 = unlines
  [ "0: 1 2"
  , "1: \"a\""
  , "2: 1 3 | 3 1"
  , "3: \"b\""
  , ""
  , "aab"
  , "aba"
  , "ab"
  , "aabb"
  ]

input2 :: String
input2 = unlines
  [ "0: 4 1 5"
  , "1: 2 3 | 3 2"
  , "2: 4 4 | 5 5"
  , "3: 4 5 | 5 4"
  , "4: \"a\""
  , "5: \"b\""
  , ""
  , "ababbb"
  , "bababa"
  , "abbbab"
  , "aaabbb"
  , "aaaabbb"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day19part1 input1 `shouldBe` "2"
      day19part1 input2 `shouldBe` "2"
