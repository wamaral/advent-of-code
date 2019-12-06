module Day3Spec where

import           Day3
import           Test.Hspec

sample1 :: String
sample1 = "R8,U5,L5,D3\n\
\U7,R6,D4,L4"

sample2 :: String
sample2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
\U62,R66,U55,R34,D71,R55,D58,R83"

sample3 :: String
sample3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n\
\U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

spec :: Spec
spec = do
  describe "part 1" $ do
    it "matches example 1" $ do
      day3part1 sample1 `shouldBe` "6"
    it "matches example 2" $ do
      day3part1 sample2 `shouldBe` "159"
    it "matches example 3" $ do
      day3part1 sample3 `shouldBe` "135"
