{-# LANGUAGE OverloadedStrings #-}

module Day5Tests where

import Day5

import TestHelpers

import Data.Vector.Unboxed as V

import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

day5Tests :: TestTree
day5Tests = testGroup "Day 5 tests"
  [ testGroup "full problem"
    [ testCase "part 1" $ part1 >>= (@?= 9219874)
    , testCase "part 2" $ part2 >>= (@?= 5893654)
    ]
  , testGroup "parseInstruction"
    [ testCase "11102" $ parseInstruction 11102 @?= Just (Multiply Immediate Immediate Immediate)
    , testCase "00002" $ parseInstruction 00002 @?= Just (Multiply Position Position Position)
    , testCase "0002" $ parseInstruction 0002 @?= Just (Multiply Position Position Position)
    , testCase "002" $ parseInstruction 002 @?= Just (Multiply Position Position Position)
    , testCase "02" $ parseInstruction 02 @?= Just (Multiply Position Position Position)
    , testCase "02" $ parseInstruction 2 @?= Just (Multiply Position Position Position)
    , testCase "1002" $ parseInstruction 1002 @?= Just (Multiply Position Immediate Position)
    , testCase "11002" $ parseInstruction 11002 @?= Just (Multiply Position Immediate Immediate)
    , testCase "1" $ parseInstruction 1 @?= Just (Add Position Position Position)

    , testCase "11102" $ parseInstruction 11101 @?= Just (Add Immediate Immediate Immediate)
    , testCase "11102" $ parseInstruction 1 @?= Just (Add Position Position Position)

    , testCase "3" $ parseInstruction 3 @?= Just Input

    , testCase "104" $ parseInstruction 104 @?= Just (Output Immediate)
    , testCase "4" $ parseInstruction 4 @?= Just (Output Position)
    ]
  , testGroup "runProgram"
    [ testCase "3,0,4,0,99" $ runProgram 1234 0 (V.fromList [3,0,4,0,99]) [] @?= [1234]
    ]
  , testGroup "runProgram2"
    [ testCase "3,9,8,9,10,9,4,9,99,-1,8" $ runProgram2 7 0 (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= 0
    , testCase "3,9,8,9,10,9,4,9,99,-1,8" $ runProgram2 8 0 (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= 1
    , testCase "3,9,8,9,10,9,4,9,99,-1,8" $ runProgram2 9 0 (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= 0

    , testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram2 7 0 (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= 1
    , testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram2 8 0 (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= 0
    , testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram2 9 0 (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= 0


    , testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram2 7 0 (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= 0
    , testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram2 8 0 (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= 1
    , testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram2 9 0 (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= 0

    , testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram2 7 0 (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= 1
    , testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram2 8 0 (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= 0
    , testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram2 9 0 (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= 0

    , testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram2 0 0 (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= 0
    , testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram2 1 0 (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= 1
    , testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram2 2 0 (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= 1

    , testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram2 0 0 (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= 0
    , testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram2 1 0 (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= 1
    , testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram2 2 0 (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= 1
    ]
  ]

