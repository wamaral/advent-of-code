module Day1Spec
  (spec)
  where

import           Day1
import           Test.Hspec

input :: String
input = unlines
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day1part1 input `shouldBe` "24000"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day1part2 input `shouldBe` "45000"
