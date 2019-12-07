{-# LANGUAGE OverloadedStrings #-}

module Day7Tests where

import qualified Day5
import Day7

import TestHelpers

import Data.Vector.Unboxed as V

import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

day7Tests :: TestTree
day7Tests = testGroup "day 7"
  [ testGroup "part 1"
    [ testCase "actual input" $ part1 >>= (@?= 929800)
    , testGroup "example steps" $
      let example1 = V.fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
          example2 = V.fromList [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
          example3 = V.fromList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
      in [ testCase "example1" $ part1' example1 @?= 43210
         , testCase "example1 max" $ runAllAmps example1 [4,3,2,1,0] @?= 43210
         , testCase "example2" $ part1' example2 @?= 54321
         , testCase "example2 max" $ runAllAmps example2 [0..4] @?= 54321
         , testCase "example3" $ part1' example3 @?= 65210
         , testCase "example3 max" $ runAllAmps example3 [1,0,4,3,2] @?= 65210
         ]
    ]
  ]
