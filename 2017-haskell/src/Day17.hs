module Day17 (part1, part2) where

import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (foldl')

-- not 996
input, numToInsert :: Int
input = 345
numToInsert = 2017
part1, part2 :: Int
part1 = part1Algorithm input numToInsert V.! 1
part2 = undefined

part1Algorithm :: Int -> Int -> V.Vector Int
part1Algorithm forwardSteps numToInsert = foldl' (\acc n -> V.cons n (stepForwardN (1 + forwardSteps) acc)) (V.singleton 0) [1..numToInsert]

stepForwardN :: Int -> V.Vector Int -> V.Vector Int
stepForwardN n v = iterate stepForward v !! n

stepForward :: V.Vector Int -> V.Vector Int
stepForward v = V.tail v `V.snoc` V.head v

-----------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 17"
  [ stepForwardTests
  ]
 where
  stepForwardTests = testGroup "step forward"
    [ testGroup "one step"
      [ testCase "0" $ stepForward (V.singleton 0) @?= V.singleton 0
      , testCase "<0,1>" $ stepForward (V.fromList [0..1]) @?= V.fromList [1,0]
      , testCase "<0,1,2>" $ stepForward (V.fromList [0..2]) @?= V.fromList [1,2,0]
      ]
    , testGroup "multiple steps"
      [ testCase "0" $ stepForwardN 4 (V.singleton 0) @?= V.singleton 0
      
      , testCase "1 step: <0,1>" $ stepForwardN 1 (V.fromList [0..1]) @?= V.fromList [1,0]
      , testCase "2 steps: <0,1>" $ stepForwardN 2 (V.fromList [0..1]) @?= V.fromList [0,1]
      , testCase "3 steps: <0,1>" $ stepForwardN 3 (V.fromList [0..1]) @?= V.fromList [1,0]
      
      , testCase "1 step: <0,1,2,3>" $ stepForwardN 1 (V.fromList [0..3]) @?= V.fromList [1,2,3,0]
      , testCase "2 steps: <0,1,2,3>" $ stepForwardN 2 (V.fromList [0..3]) @?= V.fromList [2,3,0,1]
      , testCase "3 steps: <0,1,2,3>" $ stepForwardN 3 (V.fromList [0..3]) @?= V.fromList [3,0,1,2]
      , testCase "4 steps: <0,1,2,3>" $ stepForwardN 4 (V.fromList [0..3]) @?= V.fromList [0,1,2,3]
      , testCase "5 steps: <0,1,2,3>" $ stepForwardN 5 (V.fromList [0..3]) @?= V.fromList [1,2,3,0]
      ]
    ]
