module Day1Tests where

import Day1 (combinations, day1Part1, day1Part1', day1Part2, day1Part2')
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

day1Tests :: TestTree
day1Tests =
  testGroup
    "day 1"
    [ testGroup
        "fast"
        [ testCase "part 1" $ day1Part1' testInput @?= 514579,
          testCase "part 1 handles duplicates" $ day1Part1' [1010, 1010] @?= 1020100,
          testCase "part 2" $ day1Part2' testInput @?= 241861950
        ],
      testGroup
        "slow"
        [ testCase "part 1" $ day1Part1 >>= (@?= 252724),
          testCase "part 2" $ day1Part2 >>= (@?= 276912720),
          testGroup
            "property tests"
            [ testProperty "combinations length" prop_number_of_combos,
              testProperty "combinations size" prop_size_of_combos
            ]
        ]
    ]

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]

choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 1
choose n k = choose (n - 1) (k - 1) * n `div` k

prop_number_of_combos :: Property
prop_number_of_combos = property $ do
  let top = 8
  k <- forAll (Gen.int (Range.linear 0 top))
  is <- forAll (Gen.list (Range.linear k top) (Gen.int (Range.linear 0 3000)))
  let n = length is
  max 1 (length (combinations k is)) === n `choose` k

prop_size_of_combos :: Property
prop_size_of_combos = property $ do
  let top = 8
  k <- forAll (Gen.int (Range.linear 0 top))
  is <- forAll (Gen.list (Range.linear k top) (Gen.int (Range.linear 0 3000)))
  let n = length is
  all ((== k) . length) (combinations k is) === True

genIntList :: Int -> Gen [Int]
genIntList upperBound =
  let numberGenerator = Gen.int (Range.linear 0 upperBound)
   in Gen.list (Range.linear 0 1000) numberGenerator