{-# LANGUAGE OverloadedStrings #-}

module Day6Tests where

import Day6

import TestHelpers

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

testInput1 :: [(Text, Text)]
testInput1 = [ ("COM","B")
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
            
testInput2 = [ ("COM","B")
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

day6Tests :: TestTree
day6Tests = testGroup "Day 6 tests"
  [ testGroup "part 1"
    [ testCase "sample input" $ part1' testInput1 @?= 42
    , testCase "actual input" $ part1 >>= (@?= 278744)
    ]
  , testGroup "part 2"
    [ testCase "sample input" $ part2' testInput2 @?= Just 4
    , testCase "actual input" $ part2 >>= (@?= Just 475)
    ]
  ]
