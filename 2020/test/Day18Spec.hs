module Day18Spec
  (spec)
  where

import           Day18
import           Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day18part1 "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` "71"
      day18part1 "1 + (2 * 3) + (4 * (5 + 6))" `shouldBe` "51"
      day18part1 "2 * 3 + (4 * 5)" `shouldBe` "26"
      day18part1 "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` "437"
      day18part1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe` "12240"
      day18part1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` "13632"
