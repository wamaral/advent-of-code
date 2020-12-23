module Day21Spec
  (spec)
  where

import           Day21
import           Test.Hspec

input :: String
input = unlines
  [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  , "sqjhc fvjkl (contains soy)"
  , "sqjhc mxmxvkd sbzzf (contains fish)"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day21part1 input `shouldBe` "5"
  describe "part 2" $ do
    it "runs provided example" $ do
      day21part2 input `shouldBe` "mxmxvkd,sqjhc,fvjkl"
