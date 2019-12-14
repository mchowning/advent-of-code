module Day13Tests where

import Day13

import Test.Tasty
import Test.Tasty.HUnit

day13Tests = testGroup "day 13"
  [ testCase "part 1" $ part1 >>= (@?= 335)
  ]

