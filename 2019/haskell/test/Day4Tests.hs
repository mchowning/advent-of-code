{-# LANGUAGE OverloadedStrings #-}

module Day4Tests where

import Day4

import TestHelpers

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.List (group, nub)
import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Control.Applicative

day4Tests :: TestTree
day4Tests = testGroup "Day 4 tests"
  [ testCase "part1" $ part1 @?= 889
  , testCase "part2" $ part2 @?= 589
  , testGroup "property tests"
    [  testProperty "part 1 prop" part1Prop
    ,  testProperty "part 2 prop" part2Prop
    ]
  , testGroup "passwordSatisfiesPart1Rule"
    [ testCase "222222" $ passwordSatisfiesPart1Rule 222222 @? "222222 is a valid password"
    , testCase "223456" $ passwordSatisfiesPart1Rule 223456 @? "223456 is a valid password"
    , testCase "112378" $ passwordSatisfiesPart1Rule 112378 @? "112378 is a valid password"
    , testCase "223450" $ not (passwordSatisfiesPart1Rule 223450) @? "223450 is not a valid password (not always increasing)"
    , testCase "123789" $ not (passwordSatisfiesPart1Rule 123789) @? "123789 is not a valid password (no adjacent pair)"
    ]
  , testGroup "passwordSatisfiesPart2Rule"
    [ testCase "222222" $ not (passwordSatisfiesPart2Rule 222222) @? "222222 is not a valid password"
    , testCase "223456" $ passwordSatisfiesPart2Rule 223456 @? "223456 is a valid password"
    , testCase "112233" $ passwordSatisfiesPart2Rule 112233 @? "112233 is a valid password"
    , testCase "112378" $ passwordSatisfiesPart2Rule 112378 @? "112378 is a valid password"
    , testCase "223450" $ not (passwordSatisfiesPart2Rule 223450) @? "223450 is not a valid password (not always increasing)"
    , testCase "123789" $ not (passwordSatisfiesPart2Rule 123789) @? "123789 is not a valid password (no adjacent pair)"
    , testCase "123444" $ not (passwordSatisfiesPart2Rule 123444) @? "123444 is not a valid password (adjacent set of 3)"
    , testCase "111122" $ passwordSatisfiesPart2Rule 111122 @? "111122 is a valid password (even though there are more than two adjacent 1's, there are only two adjacent 2's)"
    ]
  , testGroup "hasTwoAdjacent"
    [ testCase "112233" $ hasTwoAdjacent 112233 @? "112233 has exactly two adjacent"
    , testCase "112345" $ hasTwoAdjacent 112345 @? "112345 has exactly two adjacent"
    , testCase "123444" $ not (hasTwoAdjacent 123444) @? "123444 has more than two adjacent (444)"
    , testCase "111223" $ hasTwoAdjacent 111223 @? "111122 has more than two adjacent (even though there are more than two adjacent 1's, there are only two adjacent 2's)"
    ]
  , testGroup "hasAtLeastTwoAdjacent"
    [ testCase "111111" $ hasAtLeastTwoAdjacent 111111 @? "111111 has two adjacent"
    , testCase "529945" $ hasAtLeastTwoAdjacent 111111 @? "111111 has two adjacent"
    , testCase "123789" $ not (hasAtLeastTwoAdjacent 123789) @? "123789 does not have two adjacent"
    ]
  , testGroup "toDigits"
    [ testCase "12345" $ toDigits 12345 @?= [1..5]
    , testCase "11111" $ toDigits 11111 @?= replicate 5 1
    , testCase "8327221" $ toDigits 8327221 @?= [8, 3, 2, 7, 2, 2, 1]
    ]
  , testGroup "neverDecreases"
    [ testCase "123789" $ neverDecreases [1, 2, 3, 7, 8, 9] @? "123789 is never decreasing"
    , testCase "999999" $ neverDecreases [9, 9, 9, 9, 9, 9] @? "999999 is never decreasing"
    , testCase "223450" $ not (neverDecreases [2, 2, 3, 4, 5, 0]) @? "5 -> 0 decreases"
    ]
  ]

part1Prop = passwordProp passwordSatisfiesPart1Rule adjacencyCheck
  where adjacencyCheck n = (< length (show n)) . length . nub . show $ n

-- FIXME just repeating logic being tested
part2Prop = passwordProp passwordSatisfiesPart2Rule adjacencyCheck
  where adjacencyCheck = elem 2 . map length . group . show

passwordProp checker adjacencyRequirement = withTests 5000 . property $ do
  n <- generateSample
  let isValidPassword = checker n
  let neverDecreases = all (uncurry (<=)) (pairs (digitToInt <$> show n))
  let adjacencyCheck = adjacencyRequirement n
  annotateShow isValidPassword
  annotateShow neverDecreases
  annotateShow (Day4.neverDecreases (digitToInt <$> show n))
  annotateShow adjacencyCheck
  annotateShow (hasTwoAdjacent n)
  HH.assert $ if isValidPassword
    then neverDecreases && adjacencyCheck
    else not (neverDecreases && adjacencyCheck)

pairs :: [Int] -> [(Int, Int)]
pairs xs =
  if length xs < 2
     then []
     else (head xs, xs !! 1) : pairs (tail xs)


generateSample :: PropertyT IO Int
generateSample = forAll (Gen.int (Range.constant 100000 999999))
