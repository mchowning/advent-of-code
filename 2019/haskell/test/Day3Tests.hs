{-# LANGUAGE OverloadedStrings #-}

module Day3Tests where

import qualified Day3 as D3
import Day3

import TestHelpers

import Test.Tasty
import Test.Tasty.HUnit

day3Tests :: TestTree
day3Tests = testGroup "Day 3 tests"
    [ testCase "part 1" $ part1 >>= (@?= 721)
    , testCase "part 2" $ part2 >>= (@?= 7388)
    ,  testCase "parsing" $ testParse parser
        "L212,R333\nU343,D33"
        ([Move D3.Left 212, Move D3.Right 333], [Move Up 343, Move Down 33])
    , testGroup "findEnd"
      [ testCase "move up" $ findEnd (0,0) (Move Up 3) @?= (0,3)
      , testCase "move right" $ findEnd (0,0) (Move D3.Right 33) @?= (33,0)
      , testCase "move down" $ findEnd (0,0) (Move Down 5) @?= (0,-5)
      , testCase "move left" $ findEnd (0,0) (Move D3.Left 1) @?= (-1,0)
      ]
    , testCase "travel" $ travel (0,0) (Move Up 3) @?= [(0,1), (0,2), (0,3)]
    ]