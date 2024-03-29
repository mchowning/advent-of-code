module Day2Tests where

import Day2
import Test.Tasty
import Test.Tasty.HUnit

day2 :: TestTree
day2 =
  testGroup
    "day 2"
    [ testGroup
        "part 1"
        [ testCase "1-3 a: abcde" $ isValid1 (Input 1 3 'a' "abcde") @? "should have been valid",
          testCase "1-3 b: cdefg" $ not (isValid1 (Input 1 3 'b' "cdefg")) @? "should not have been valid",
          testCase "2-9 c: ccccccccc" $ isValid1 (Input 2 9 'c' "ccccccccc") @? "should not have been valid",
          testCase "full" $ part1 >>= (@?= 378)
        ],
      testGroup
        "part 2"
        [ testCase "1-3 a: abcde" $ isValid2 (Input 1 3 'a' "abcde") @? "should have been valid",
          testCase "1-3 b: cdefg" $ not (isValid2 (Input 1 3 'b' "cdefg")) @? "should not have been valid",
          testCase "2-9 c: ccccccccc" $ not (isValid2 (Input 2 9 'c' "ccccccccc")) @? "should not have been valid",
          testCase "full" $ part2 >>= (@?= 280)
        ]
    ]