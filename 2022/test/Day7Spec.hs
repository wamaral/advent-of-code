module Day7Spec
  (spec)
  where

import           Day7
import           Test.Hspec

input :: String
input = unlines
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided examples" $ do
      day7part1 input `shouldBe` "95437"
  describe "part 2" $ do
    it "runs provided examples" $ do
      day7part2 input `shouldBe` "24933642"
