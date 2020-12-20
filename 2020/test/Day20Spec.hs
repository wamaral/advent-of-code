module Day20Spec
  (spec)
  where

import           Day20
import           Test.Hspec

input :: String
input = unlines
  [ "Tile 2311:"
  , "..##.#..#."
  , "##..#....."
  , "#...##..#."
  , "####.#...#"
  , "##.##.###."
  , "##...#.###"
  , ".#.#.#..##"
  , "..#....#.."
  , "###...#.#."
  , "..###..###"
  , ""
  , "Tile 1951:"
  , "#.##...##."
  , "#.####...#"
  , ".....#..##"
  , "#...######"
  , ".##.#....#"
  , ".###.#####"
  , "###.##.##."
  , ".###....#."
  , "..#.#..#.#"
  , "#...##.#.."
  , ""
  , "Tile 1171:"
  , "####...##."
  , "#..##.#..#"
  , "##.#..#.#."
  , ".###.####."
  , "..###.####"
  , ".##....##."
  , ".#...####."
  , "#.##.####."
  , "####..#..."
  , ".....##..."
  , ""
  , "Tile 1427:"
  , "###.##.#.."
  , ".#..#.##.."
  , ".#.##.#..#"
  , "#.#.#.##.#"
  , "....#...##"
  , "...##..##."
  , "...#.#####"
  , ".#.####.#."
  , "..#..###.#"
  , "..##.#..#."
  , ""
  , "Tile 1489:"
  , "##.#.#...."
  , "..##...#.."
  , ".##..##..."
  , "..#...#..."
  , "#####...#."
  , "#..#.#.#.#"
  , "...#.#.#.."
  , "##.#...##."
  , "..##.##.##"
  , "###.##.#.."
  , ""
  , "Tile 2473:"
  , "#....####."
  , "#..#.##..."
  , "#.##..#..."
  , "######.#.#"
  , ".#...#.#.#"
  , ".#########"
  , ".###.#..#."
  , "########.#"
  , "##...##.#."
  , "..###.#.#."
  , ""
  , "Tile 2971:"
  , "..#.#....#"
  , "#...###..."
  , "#.#.###..."
  , "##.##..#.."
  , ".#####..##"
  , ".#..####.#"
  , "#..#.#..#."
  , "..####.###"
  , "..#.#.###."
  , "...#.#.#.#"
  , ""
  , "Tile 2729:"
  , "...#.#.#.#"
  , "####.#...."
  , "..#.#....."
  , "....#..#.#"
  , ".##..##.#."
  , ".#.####..."
  , "####.#.#.."
  , "##.####..."
  , "##..#.##.."
  , "#.##...##."
  , ""
  , "Tile 3079:"
  , "#.#.#####."
  , ".#..######"
  , "..#......."
  , "######...."
  , "####.#..#."
  , ".#...#.##."
  , "#.#####.##"
  , "..#.###..."
  , "..#......."
  , "..#.###..."
  ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "runs provided example" $ do
      day20part1 input `shouldBe` "20899048083289"
  describe "part 2" $ do
    it "runs provided example" $ do
      day20part2 input `shouldBe` "273"
