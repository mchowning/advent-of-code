module Day17 (part1, part2) where

import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (foldl')

newtype Index = Index Int deriving Show
newtype NumSteps = NumSteps Int deriving Show

input :: NumSteps
input = NumSteps 345
part1, part2 :: Int
part1 = part1Algorithm input 2017 V.! 1
part2 = let (n, _, _, _, _) = part2Algorithm (NumSteps 345) 50000000 in n

part1Algorithm :: NumSteps -> Int -> V.Vector Int
part1Algorithm (NumSteps forwardSteps) numToInsert = foldl' (\acc n -> V.cons n (stepForwardN (1 + forwardSteps) acc)) (V.singleton 0) [1..numToInsert]

stepForwardN :: Int -> V.Vector Int -> V.Vector Int
stepForwardN n v = iterate stepForward v !! n

stepForward :: V.Vector Int -> V.Vector Int
stepForward v = V.tail v `V.snoc` V.head v

lastAfter0 :: (Int, Int, Index, NumSteps, Int) -> (Int, Int, Index, NumSteps, Int)
lastAfter0 (lastAfter0Value, currentNumValue, Index index, NumSteps numSteps, length) =
  let insertAfterPoint = (index + numSteps) `mod` length
      newLastAfter0Value =
        if insertAfterPoint == 0
          then currentNumValue
          else lastAfter0Value
  in (newLastAfter0Value, 1 + currentNumValue, Index (1 + insertAfterPoint), NumSteps numSteps, 1 + length)
  
part2Algorithm :: NumSteps -> Int -> (Int, Int, Index, NumSteps, Int)
part2Algorithm numSteps numDances = iterate lastAfter0 (-1, 1, Index 0, numSteps, 1) !! numDances


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
