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
    -- FIXME I think these were faulty tests
--  [ testGroup "parseInstruction"
--    [ testCase "11102" $ parseInstruction 11102 @?= Just (Multiply Immediate Immediate Immediate)
--    , testCase "00002" $ parseInstruction 00002 @?= Just (Multiply Position Position Position)
--    [ testCase "0002" $ parseInstruction 0002 @?= Just (Multiply Position Position Position)
--    , testCase "002" $ parseInstruction 002 @?= Just (Multiply Position Position Position)
--    , testCase "02" $ parseInstruction 02 @?= Just (Multiply Position Position Position)
--    , testCase "02" $ parseInstruction 2 @?= Just (Multiply Position Position Position)
--    , testCase "1002" $ parseInstruction 1002 @?= Just (Multiply Position Immediate Position)
--    , testCase "11002" $ parseInstruction 11002 @?= Just (Multiply Position Immediate Immediate)
--    , testCase "1" $ parseInstruction 1 @?= Just (Add Position Position Position)
--
--    , testCase "11102" $ parseInstruction 11101 @?= Just (Add Immediate Immediate Immediate)
--    , testCase "11102" $ parseInstruction 1 @?= Just (Add Position Position Position)
--
--    , testCase "3" $ parseInstruction 3 @?= Just (Input Immediate)
--
--    , testCase "104" $ parseInstruction 104 @?= Just (Output Immediate)
--    , testCase "4" $ parseInstruction 4 @?= Just (Output Position)
--    ]
  , testGroup "runProgram2"
    [
    testGroup "Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)"
      [ testCase "input 7" $ runProgram True [7] (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= [0]
      , testCase "input 8" $ runProgram True [8] (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= [1]
      , testCase "input 9" $ runProgram True [9] (V.fromList [3,9,8,9,10,9,4,9,99,-1,8]) @?= [0]
      ]

    , testGroup "Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not)"
      [ testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram True [7] (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= [1]
      , testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram True [8] (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= [0]
      , testCase "3,9,7,9,10,9,4,9,99,-1,8" $ runProgram True [9] (V.fromList [3,9,7,9,10,9,4,9,99,-1,8]) @?= [0]
      ]

    , testGroup "Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)"
      [ testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram True [7] (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= [0]
      , testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram True [8] (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= [1]
      , testCase "3,3,1108,-1,8,3,4,3,99" $ runProgram True [9] (V.fromList [3,3,1108,-1,8,3,4,3,99]) @?= [0]
      ]

    , testGroup "Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not)"
      [ testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram True [7] (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= [1]
      , testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram True [8] (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= [0]
      , testCase "3,3,1107,-1,8,3,4,3,99" $ runProgram True [9] (V.fromList [3,3,1107,-1,8,3,4,3,99]) @?= [0]
      ]

    , testGroup "Using position mode take an input, then output 0 if the input was zero or 1 if the input was non-zero"
      [ testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram True [0] (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= [0]
      , testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram True [1] (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= [1]
      , testCase "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" $ runProgram True [2] (V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) @?= [1]
      ]

    , testGroup "Using immediate mode take an input, then output 0 if the input was zero or 1 if the input was non-zero"
      [ testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram True [0] (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= [0]
      , testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram True [1] (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= [1]
      , testCase "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" $ runProgram True [2] (V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) @?= [1]
      ]

    , let v = V.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
      in testGroup "output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8"
        [ testCase "7" $ runProgram True [7] v @?= [999]
        , testCase "8" $ runProgram True [8] v @?= [1000]
        , testCase "9" $ runProgram True [9] v @?= [1001]
        ]
    ]
  ]

