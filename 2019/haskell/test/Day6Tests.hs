{-# LANGUAGE OverloadedStrings #-}

module Day6Tests where

import Day6

import Test.Tasty
import Test.Tasty.HUnit

day6Tests :: TestTree
day6Tests = testGroup "Day 6 tests"
  [ testGroup "part 1" $
    let testInput1 = [ ("COM","B")
                     , ("B","C")
                     , ("C","D")
                     , ("D","E")
                     , ("E","F")
                     , ("B","G")
                     , ("G","H")
                     , ("D","I")
                     , ("E","J")
                     , ("J","K")
                     , ("K","L")
                     ]
    in [ testCase "sample input" $ part1' testInput1 @?= 42
       , testCase "actual input" $ part1 >>= (@?= 278744)
       ]
  , testGroup "part 2" $
      let testInput2 = [ ("COM","B")
                       , ("B","C")
                       , ("C","D")
                       , ("D","E")
                       , ("E","F")
                       , ("B","G")
                       , ("G","H")
                       , ("D","I")
                       , ("E","J")
                       , ("J","K")
                       , ("K","L")
                       , ("K","YOU")
                       , ("I","SAN")
                       ]
    in [ testCase "sample input" $ part2' testInput2 @?= Just 4
       , testCase "actual input" $ part2 >>= (@?= Just 475)
       ]
  ]
