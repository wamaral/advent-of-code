module Day3Spec
  (spec)
  where

import           Day3
import           Test.Hspec

input :: String
input = unlines
  [ "467..114.."
  , "...*......"
  , "..35..633."
  , "......#..."
  , "617*......"
  , ".....+.58."
  , "..592....."
  , "......755."
  , "...$.*...."
  , ".664.598.."
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day3part1 input `shouldBe` "4361"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day3part2 input `shouldBe` "467835"
