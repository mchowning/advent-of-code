{-# LANGUAGE OverloadedStrings #-}

module Day2Tests where

import Day2

import Test.Tasty
import Test.Tasty.HUnit

day2Tests :: TestTree
day2Tests = testGroup "day 2"
  [ testCase "part 1" $ part1 >>= (@?= 3101844)
  , testCase "part 2" $ part2 >>= (@?= 8478)
  ]