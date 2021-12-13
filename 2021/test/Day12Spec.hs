module Day12Spec
  (spec)
  where

import           Day12
import           Test.Hspec

input :: String
input = unlines
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

input2 :: String
input2 = unlines
  [ "dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"
  ]

input3 :: String
input3 = unlines
  [ "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day12part1 input `shouldBe` "10"
      day12part1 input2 `shouldBe` "19"
      day12part1 input3 `shouldBe` "226"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day12part2 input `shouldBe` ""
