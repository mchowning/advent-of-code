module Day3Bench where

import Day3
import Test.Tasty.Bench

day3 :: Benchmark
day3 = bgroup "day 3"
  [ bench "part 1" $ nfIO part1
--   , bench "part 2" $ nfIO part2
  ]