module Day25Spec
  (spec)
  where

import           Day25
import           Test.Hspec

input :: String
input = unlines
  [ "5764801"
  , "17807724"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day25part1 input `shouldBe` "14897079"
  describe "part 2" $ do
    it "runs provided example" $ do
      day25part2 "" `shouldBe` ""
