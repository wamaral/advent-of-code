module Day24Spec
  (spec)
  where

import           Day24
import           Test.Hspec

input :: String
input = unlines
  [ "sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day24part1 input `shouldBe` "10"
  describe "part 2" $ do
    it "runs provided example" $ do
      day24part2 "" `shouldBe` ""
