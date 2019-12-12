module Day11Tests where

import Day11

import Test.Tasty
import Test.Tasty.HUnit


day11Tests :: TestTree
day11Tests = testGroup "day 11"
  [ testCase "part 1" $ part1 >>= (@?= 2056)
  ]
