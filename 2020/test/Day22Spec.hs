module Day22Spec
  (spec)
  where

import           Day22
import           Test.Hspec

input :: String
input = unlines
  [ "Player 1:"
  , "9"
  , "2"
  , "6"
  , "3"
  , "1"
  , ""
  , "Player 2:"
  , "5"
  , "8"
  , "4"
  , "7"
  , "10"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day22part1 input `shouldBe` "306"
  describe "part 2" $ do
    it "runs provided example" $ do
      day22part2 "" `shouldBe` ""
