module Day9Tests where

import Day9

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Test.Tasty
import Test.Tasty.HUnit

day9Tests :: TestTree
day9Tests = testGroup "day 9"
  [ testGroup "part 1" $
    let example1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
        example2 = [1102,34915192,34915192,7,4,7,99,0]
        example3 = [104,1125899906842624,99]
    in [ testCase "part 1 actual input" $ part1 >>= (@?= 2682107844)
       , testCase "part 2 actual input" $ part2 >>= (@?= 34738)
       , testCase "example 1" $ ampTest 1 example1 @?= example1
       , testCase "example 2" $ length (show (head (ampTest 1 example2))) @?= 16
       , testCase "example 3" $ head (ampTest 1 example3) @?= (example3 !! 1)

       , testCase "my test i/o" $ ampTest 1 [109, 3, 203, 7, 4, 10, 99] @?= [1]

       , testCase "1" $ ampTest 1 [109, -1, 4, 1, 99] @?= [-1]
       , testCase "2" $ ampTest 1 [109, -1, 104, 1, 99] @?= [1]
       , testCase "3" $ ampTest 1 [109, -1, 204, 1, 99] @?= [109]
       , testCase "4" $ ampTest 1 [109, 1, 9, 2, 204, -6, 99] @?= [204]
       , testCase "5" $ ampTest 1 [109, 1, 209, -1, 204, -106, 99] @?= [204]
       , testCase "6" $ ampTest 1 [109, 1, 3, 3, 204, 2, 99] @?= [1]
       , testCase "7" $ ampTest 1 [109, 1, 203, 2, 204, 2, 99] @?= [1]
       ]
  ]

