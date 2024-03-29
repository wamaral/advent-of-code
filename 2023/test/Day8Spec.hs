module Day8Spec
  (spec)
  where

import           Day8
import           Test.Hspec

input :: String
input = unlines
  [ "RL"
  , ""
  , "AAA = (BBB, CCC)"
  , "BBB = (DDD, EEE)"
  , "CCC = (ZZZ, GGG)"
  , "DDD = (DDD, DDD)"
  , "EEE = (EEE, EEE)"
  , "GGG = (GGG, GGG)"
  , "ZZZ = (ZZZ, ZZZ)"
  ]

input2 :: String
input2 = unlines
  [ "LLR"
  , ""
  , "AAA = (BBB, BBB)"
  , "BBB = (AAA, ZZZ)"
  , "ZZZ = (ZZZ, ZZZ)"
  ]

-- tr 12 FG
input3 :: String
input3 = unlines
  [ "LR"
  , ""
  , "FFA = (FFB, XXX)"
  , "FFB = (XXX, FFZ)"
  , "FFZ = (FFB, XXX)"
  , "GGA = (GGB, XXX)"
  , "GGB = (GGC, GGC)"
  , "GGC = (GGZ, GGZ)"
  , "GGZ = (GGB, GGB)"
  , "XXX = (XXX, XXX)"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day8part1 input `shouldBe` "2"
      day8part1 input2 `shouldBe` "6"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day8part2 input3 `shouldBe` "6"
