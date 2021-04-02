module Day1Bench where

import Day1
import Test.Tasty.Bench

day1 :: Benchmark
day1 =
  bgroup
    "day 1"
    [ bench "part 1" $ nf part1' testInput,
      bench "part 2" $ nf part2' testInput
    ]

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]