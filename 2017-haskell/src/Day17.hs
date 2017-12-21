{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day17 (part1, part2) where

import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (foldl')

type Index = Int
type NumSteps = Int

input :: NumSteps
input = 345
part1, part2 :: Int
part1 = part1Algorithm input 2017 V.! 1
part2 = part2Algorithm 345 50000000

part1Algorithm :: NumSteps -> Int -> V.Vector Int
part1Algorithm forwardSteps numToInsert =
  -- foldl' (\acc n -> V.cons n (rotateForward (1 + forwardSteps) acc)) (V.singleton 0) [1..numToInsert]
  foldl' (flip V.cons . rotateForward (1 + forwardSteps)) (V.singleton 0) [1..numToInsert]

rotateForward :: Int -> V.Vector Int -> V.Vector Int
rotateForward n v =
  let (l,r) = V.splitAt (n `mod` V.length v) v
  in r V.++ l

part2Algorithm :: NumSteps -> Int -> Int
part2Algorithm numSteps numDances =
  fst $ foldl' lastAfter0 (-1,0) [1..numDances]
 where
  lastAfter0 :: (Int, Index) -> Int -> (Int, Index)
  lastAfter0 (!lastAfter0Value, index) currentNumValue =
    let insertAfterPoint = (index + numSteps) `mod` currentNumValue
        newLastAfter0Value =
          if insertAfterPoint == 0
            then currentNumValue
            else lastAfter0Value
    in (newLastAfter0Value, 1 + insertAfterPoint)


-----------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 17"
  [ stepForwardTests
  , part1Test
  , part2AlgorithmTests
  ]
 where
  stepForwardTests = testGroup "rotate forward"
    [ testCase "0" $ rotateForward 4 (V.singleton 0) @?= V.singleton 0
    
    , testCase "1 step: <0,1>" $ rotateForward 1 (V.fromList [0..1]) @?= V.fromList [1,0]
    , testCase "2 steps: <0,1>" $ rotateForward 2 (V.fromList [0..1]) @?= V.fromList [0,1]
    , testCase "3 steps: <0,1>" $ rotateForward 3 (V.fromList [0..1]) @?= V.fromList [1,0]
    
    , testCase "1 step: <0,1,2,3>" $ rotateForward 1 (V.fromList [0..3]) @?= V.fromList [1,2,3,0]
    , testCase "2 steps: <0,1,2,3>" $ rotateForward 2 (V.fromList [0..3]) @?= V.fromList [2,3,0,1]
    , testCase "3 steps: <0,1,2,3>" $ rotateForward 3 (V.fromList [0..3]) @?= V.fromList [3,0,1,2]
    , testCase "4 steps: <0,1,2,3>" $ rotateForward 4 (V.fromList [0..3]) @?= V.fromList [0,1,2,3]
    , testCase "5 steps: <0,1,2,3>" $ rotateForward 5 (V.fromList [0..3]) @?= V.fromList [1,2,3,0]
    ]
  part1Test = testCase "part 1" $ part1 @?= 866
  part2AlgorithmTests = testGroup "part 2 algorithm"
    [ testCase "1 round" $ part2Algorithm 3 1 @?= 1
    , testCase "2 rounds" $ part2Algorithm 3 2 @?= 2
    , testCase "3 rounds" $ part2Algorithm 3 3 @?= 2
    , testCase "4 rounds" $ part2Algorithm 3 4 @?= 2
    , testCase "5 rounds" $ part2Algorithm 3 5 @?= 5
    -- , testCase "input" $ part2 @?= 11995607 -- rather slow in repl
    ]
