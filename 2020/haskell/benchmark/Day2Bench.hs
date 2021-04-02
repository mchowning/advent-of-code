module Day2Bench where

import Day2
import Test.Tasty.Bench

day2 :: Benchmark
day2 = bgroup "day 2"
  [ bench "part 1" $ nfIO part1
  , bench "part 2" $ nfIO part2
  ]