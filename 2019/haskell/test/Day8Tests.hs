{-# LANGUAGE OverloadedStrings #-}
module Day8Tests where

import qualified Day5
import Day8

import TestHelpers

import Data.Char (digitToInt)
import           Data.List            (transpose)
import           Data.List.Split      (chunksOf)

import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

day8Tests :: TestTree
day8Tests = testGroup "day 8"
  [ testCase "part 1" $ part1 >>= (@?= 1848)
  , testGroup "part 2"
    [ testCase "actual input displays \"FGJUZ\"" $ part2 False >>= (@?= toPixel . digitToInt <$> "111100110000110100101111010000100100001010010000101110010000000101001000100100001011000010100100100010000100101001010010100001000001110011000110011110")
    , testCase "example" $ (fmap mconcat . transpose . chunksOf 4 . fmap toPixel $ [0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0]) @?= (toPixel . digitToInt) <$> "0110"
    , testGroup "findVisible"
      [ testCase "[0]" $ mconcat (toPixel <$> [0]) @?= toPixel 0
      , testCase "[1]" $ mconcat (toPixel <$> [1]) @?= toPixel 1
      , testCase "[2]" $ mconcat (toPixel <$> [2]) @?= toPixel 2

      , testCase "[2,0]" $ mconcat (toPixel <$> [2,0]) @?= toPixel 0
      , testCase "[2,1]" $ mconcat (toPixel <$> [2,1]) @?= toPixel 1
      , testCase "[2,2]" $ mconcat (toPixel <$> [2,2]) @?= toPixel 2

      , testCase "[0,1]" $ mconcat (toPixel <$> [0,1]) @?= toPixel 0
      , testCase "[1,0]" $ mconcat (toPixel <$> [1,0]) @?= toPixel 1

      , testCase "[1,2]" $ mconcat (toPixel <$> [1,2]) @?= toPixel 1
      , testCase "[0,2]" $ mconcat (toPixel <$> [0,2]) @?= toPixel 0

      , testCase "[2,2,0,1]" $ mconcat (toPixel <$> [2,2,0,1]) @?= toPixel 0
      ]
    ]
  ]
