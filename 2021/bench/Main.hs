module Main
  (main)
  where

import           Common
import           Criterion.Main
import           Day1
import           Day10
import           Day11
import           Day12
import           Day13
import           Day14
import           Day15
import           Day16
import           Day17
import           Day18
import           Day19
import           Day2
import           Day20
import           Day21
import           Day22
import           Day23
import           Day24
import           Day25
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9

main :: IO ()
main = defaultMain
  [ env (readInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf day1part1 input
      , bench "part 2" $ nf day1part2 input
      ]
  , env (readInput 2) $ \input -> bgroup "Day 2"
      [ bench "part 1" $ nf day2part1 input
      , bench "part 2" $ nf day2part2 input
      ]
  , env (readInput 3) $ \input -> bgroup "Day 3"
      [ bench "part 1" $ nf day3part1 input
      , bench "part 2" $ nf day3part2 input
      ]
  , env (readInput 4) $ \input -> bgroup "Day 4"
      [ bench "part 1" $ nf day4part1 input
      , bench "part 2" $ nf day4part2 input
      ]
  , env (readInput 5) $ \input -> bgroup "Day 5"
      [ bench "part 1" $ nf day5part1 input
      , bench "part 2" $ nf day5part2 input
      ]
  , env (readInput 6) $ \input -> bgroup "Day 6"
      [ bench "part 1" $ nf day6part1 input
      , bench "part 2" $ nf day6part2 input
      ]
  , env (readInput 7) $ \input -> bgroup "Day 7"
      [ bench "part 1" $ nf day7part1 input
      , bench "part 2" $ nf day7part2 input
      ]
  , env (readInput 8) $ \input -> bgroup "Day 8"
      [ bench "part 1" $ nf day8part1 input
      , bench "part 2" $ nf day8part2 input
      ]
  , env (readInput 9) $ \input -> bgroup "Day 9"
      [ bench "part 1" $ nf day9part1 input
      , bench "part 2" $ nf day9part2 input
      ]
  , env (readInput 10) $ \input -> bgroup "Day 10"
      [ bench "part 1" $ nf day10part1 input
      , bench "part 2" $ nf day10part2 input
      ]
  , env (readInput 11) $ \input -> bgroup "Day 11"
      [ bench "part 1" $ nf day11part1 input
      , bench "part 2" $ nf day11part2 input
      ]
  -- , env (readInput 12) $ \input -> bgroup "Day 12"
  --     [ bench "part 1" $ nf day12part1 input
  --     , bench "part 2" $ nf day12part2 input
  --     ]
  -- , env (readInput 13) $ \input -> bgroup "Day 13"
  --     [ bench "part 1" $ nf day13part1 input
  --     , bench "part 2" $ nf day13part2 input
  --     ]
  -- , env (readInput 14) $ \input -> bgroup "Day 14"
  --     [ bench "part 1" $ nf day14part1 input
  --     , bench "part 2" $ nf day14part2 input
  --     ]
  -- , env (readInput 15) $ \input -> bgroup "Day 15"
  --     [ bench "part 1" $ nf day15part1 input
  --     , bench "part 2" $ nf day15part2 input
  --     ]
  -- , env (readInput 16) $ \input -> bgroup "Day 16"
  --     [ bench "part 1" $ nf day16part1 input
  --     , bench "part 2" $ nf day16part2 input
  --     ]
  -- , env (readInput 17) $ \input -> bgroup "Day 17"
  --     [ bench "part 1" $ nf day17part1 input
  --     , bench "part 2" $ nf day17part2 input
  --     ]
  -- , env (readInput 18) $ \input -> bgroup "Day 18"
  --     [ bench "part 1" $ nf day18part1 input
  --     , bench "part 2" $ nf day18part2 input
  --     ]
  -- , env (readInput 19) $ \input -> bgroup "Day 19"
  --     [ bench "part 1" $ nf day19part1 input
  --     , bench "part 2" $ nf day19part2 input
  --     ]
  -- , env (readInput 20) $ \input -> bgroup "Day 20"
  --     [ bench "part 1" $ nf day20part1 input
  --     , bench "part 2" $ nf day20part2 input
  --     ]
  -- , env (readInput 21) $ \input -> bgroup "Day 21"
  --     [ bench "part 1" $ nf day21part1 input
  --     , bench "part 2" $ nf day21part2 input
  --     ]
  -- , env (readInput 22) $ \input -> bgroup "Day 22"
  --     [ bench "part 1" $ nf day22part1 input
  --     , bench "part 2" $ nf day22part2 input
  --     ]
  -- , env (readInput 23) $ \input -> bgroup "Day 23"
  --     [ bench "part 1" $ nf day23part1 input
  --     , bench "part 2" $ nf day23part2 input
  --     ]
  -- , env (readInput 24) $ \input -> bgroup "Day 24"
  --     [ bench "part 1" $ nf day24part1 input
  --     , bench "part 2" $ nf day24part2 input
  --     ]
  -- , env (readInput 25) $ \input -> bgroup "Day 25"
  --     [ bench "part 1" $ nf day25part1 input
  --     , bench "part 2" $ nf day25part2 input
  --     ]
  ]
