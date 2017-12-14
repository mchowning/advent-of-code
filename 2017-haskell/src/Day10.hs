{-# LANGUAGE OverloadedStrings #-}
module Day10 (part1, part2, knotHash)where

--import qualified Text.Megaparsec as P
--import qualified Text.Megaparsec.Char as P
import Data.List (reverse, foldl')
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
part2 = knotHash <$> input2

input1 :: IO [Int]
input1 = parse <$> TIO.readFile "src/input_day10.txt"

input2 :: IO String
input2 = IO.readFile "src/input_day10.txt"

knotHash :: String -> String
knotHash = makeDenseHash . part1Algo 256 . concat . replicate 64 . (++ [17, 31, 73, 47, 23]) . map ord

makeDenseHash :: [Int] -> String
makeDenseHash = concatMap getHex . xor16
  where
    xor16 :: [Int] -> [Int]
    xor16 = map (foldr1 xor) . chunksOf 16
    
    getHex :: Int -> String
    getHex n
      | n < 16    = '0' : showHex n ""
      | otherwise = showHex n ""
      

parse :: Text -> [Int]
parse = head . match (decimal `sepBy1` ",")

part1Algo :: Int -> [Int] -> [Int]
part1Algo size lengths =
  let (_,_,xs) = foldl' helper (0, 0, [0..(size-1)]) lengths
  in xs
  where
    helper (startPos, skipSize, acc) lengthOf =
      let newXs = reverseNext startPos lengthOf acc
          newStartPos = (startPos + lengthOf + skipSize) `mod` length acc
      in (newStartPos, 1+skipSize, newXs)

reverseNext :: Int -> Int -> [Int] -> [Int]
reverseNext startAt lengthOf ls =
  if startAt + lengthOf < length ls
  then
    let start = take startAt ls
        mid = reverse . take lengthOf . drop startAt $ ls
        end = drop (lengthOf + startAt) ls
    in start ++ mid ++ end
  else
    let end = take lengthOf . drop startAt $ ls
        wrappedLength = (startAt + lengthOf) - length ls
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
  , knotHashTest ]
  where
    reverseNextTests = testGroup "reverseNext"
      [ testCase "simple" $ reverseNext 1 2 [1..4] @?= [1,3,2,4]
      , testCase "wrap" $ reverseNext 3 2 [1..4] @?= [4,2,3,1]
      , testCase "longer wrap" $ reverseNext 4 5 [1..6] @?= [1,6,5,4,3,2] ]
    part1AlgoTest = testGroup "part 1 algorithm"
      [ testCase "sample input" $ part1Algo 5 [3,4,1,5] @?= [3,4,2,1,0]
      , testCase "real input" $ part1 >>= (@?= 13760) ]
    knotHashTest = testGroup "part 2 algorighm"
      [ testCase "empty string" $ knotHash "" @?= "a2582a3a0e66e6e86e3812dcb672a272"
      , testCase "AoC 2017" $ knotHash "AoC 2017" @?= "33efeb34ea91902bb2f59c9920caa6cd"
      , testCase "1,2,3" $ knotHash "1,2,3" @?= "3efbe78a8d82f29979031a4aa0b16a9d"
      , testCase "1,2,4" $ knotHash "1,2,4" @?= "63960835bcdc130f0b66d7ff4f6a5a8e"
      , testCase "actual input" $ part2 >>= (@?= "2da93395f1a6bb3472203252e3b17fe5") ]
