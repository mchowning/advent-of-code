{-# LANGUAGE OverloadedStrings #-}
module Day10 where

--import qualified Text.Megaparsec as P
--import qualified Text.Megaparsec.Char as P
import Control.Monad.State
import Data.List (reverse)
import Turtle.Pattern (decimal, match, sepBy1)
import Data.Text (Text)
import Data.Text.IO as TIO
import Data.Char (ord)
import Data.Bits (xor)
import Data.List.Split (chunksOf)
import qualified System.IO as IO
import Numeric (showHex)

import           Test.Tasty
import           Test.Tasty.HUnit

part1 :: IO Int
part1 = do
  (a:b:_) <- part1Algo 256 <$> input1
  return (a*b)

part2 :: IO String
part2 = part2Algo <$> input2

input1 :: IO [Int]
input1 = parse <$> TIO.readFile "src/input_day10.txt"

input2 :: IO [Int]
input2 = map ord <$> IO.readFile "src/input_day10.txt"

part2Algo :: [Int] -> String
part2Algo = makeDenseHash . part1Algo 256 . concat . replicate 64 . (++ [17, 31, 73, 47, 23])

makeDenseHash :: [Int] -> String
makeDenseHash = concatMap (`showHex` "") . xor16
  where
    xor16 :: [Int] -> [Int]
    xor16 = map (foldr1 xor) . chunksOf 16

parse :: Text -> [Int]
parse = head . match (decimal `sepBy1` ",")

part1Algo :: Int -> [Int] -> [Int]
part1Algo size lengths = let (_, (_,_,xs)) = runState (process lengths) (0,0,[0..(size-1)])
                         in xs

process ::  [Int] -> State (Int, Int, [Int]) [Int]
process [] = return []
process (lengthOf:ls) = do
  tup <- get
  let (startPos, skipSize, xs) = tup
      newXs = reverseNext startPos lengthOf xs
      newStartPos = (startPos + lengthOf + skipSize) `mod` length xs
  put (newStartPos, 1 + skipSize, newXs)
  process ls

reverseNext :: Int -> Int -> [Int] -> [Int]
reverseNext startAt lengthOf ls =
  if startAt + lengthOf < length ls
  then let start = take startAt ls
           mid = reverse . take lengthOf . drop startAt $ ls
           end = drop (lengthOf + startAt) ls
       in start ++ mid ++ end
  else let end = take lengthOf . drop startAt $ ls
           wrappedLength = startAt + lengthOf - length ls
           start = take wrappedLength ls
           reversed = reverse $ end ++ start
           (newEnd, newStart) = splitAt (length ls - startAt) reversed
           newMid = take (startAt - wrappedLength) (drop wrappedLength ls)
       in newStart ++ newMid ++ newEnd

-------------------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 10 tests"
  [ reverseNextTests
  , part1AlgoTest
  , part2AlgoTest
  ]
  where
    reverseNextTests = testGroup "reverseNext"
      [ testCase "simple" $ reverseNext 1 2 [1..4] @?= [1,3,2,4]
      , testCase "wrap" $ reverseNext 3 2 [1..4] @?= [4,2,3,1]
      , testCase "longer wrap" $ reverseNext 4 5 [1..6] @?= [1,6,5,4,3,2]
      ]
    part1AlgoTest = testGroup "part 1 algorithm"
      [ testCase "sample input" $ part1Algo 5 [3,4,1,5] @?= [3,4,2,1,0]
      , testCase "real input" $ part1 >>= (@?= 13760)
      ]
    part2AlgoTest = testGroup "part 2 algorighm"
      [ testCase "sample input" $ part2Algo [3,4,1,5] @?= "933a4c80ba5da49858041c881c992e"
      , testCase "actual input" $ part2 >>= (@?= "2da93395f1a6bb3472203252e3b17fe5")
      ]
