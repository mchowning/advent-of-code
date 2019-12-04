{-# LANGUAGE OverloadedStrings #-}

module Day1Tests where

import Day1

import Test.Tasty
import Test.Tasty.HUnit

day1Tests :: TestTree
day1Tests = testGroup "day 1"
  [ testCase "part 1" $ part1 >>= (@?= 3299598 )
  , testCase "part 2" $ part2 >>= (@?= 4946546)
  ]