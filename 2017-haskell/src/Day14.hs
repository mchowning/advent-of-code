{-# LANGUAGE LambdaCase #-}
module Day14 (part1, part2) where

import Day10 (knotHash)

import Test.Tasty
import Test.Tasty.HUnit

part1 :: Int
part1 = part1Algorithm input

part2 :: Integer
part2 = undefined

input :: String
input = "wenycdww"

part1Algorithm :: String -> Int
part1Algorithm s = length . filter binToBool . concatMap (rowBinaryRepresentation . (++) s . (++) "-" . show) $ [0 .. 127]

rowBinaryRepresentation :: String -> String
rowBinaryRepresentation = concatMap hexToBinary . knotHash

-- FIXME
hexToBinary :: Char -> String
hexToBinary = \case
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'a' -> "1010"
  'b' -> "1011"
  'c' -> "1100"
  'd' -> "1101"
  'e' -> "1110"
  'f' -> "1111"
  x   -> error ("invalid hex digit: " ++ show x)
  
binToBool :: Char -> Bool
binToBool '0' = False
binToBool '1' = True
binToBool x = error ("invalid bin digit: " ++ show x)


-------------------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 14"
  [ part1AlgorithmTests
  ]
 where
  part1AlgorithmTests = testGroup "part 1 algorithm"
    [ testCase "sample input: flqrgnkx" $ part1Algorithm "flqrgnkx" @?= 8108
    , testCase ("real input: " ++ input) $ part1Algorithm input @?= 8226
    ]
