module Day13Spec
  (spec)
  where

import           Day13
import           Test.Hspec

input :: String
input = unlines
  [ "939"
  , "7,13,x,x,59,x,31,19"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day13part1 input `shouldBe` "295"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day13part2 input `shouldBe` "1068781"
      day13part2 "1\n17,x,13,19" `shouldBe` "3417"
      day13part2 "1\n67,7,59,61" `shouldBe` "754018"
      day13part2 "1\n67,x,7,59,61" `shouldBe` "779210"
      day13part2 "1\n67,7,x,59,61" `shouldBe` "1261476"
      day13part2 "1\n1789,37,47,1889" `shouldBe` "1202161486"
