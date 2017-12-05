module Day2 (part1, part2) where

import           Control.Applicative (liftA2)
import           Data.List           (tails)
import           Data.Maybe          (fromJust, isJust)
import           Data.Monoid         ((<>))
import           Test.Tasty
import           Test.Tasty.HUnit

part1 :: IO Integer
part1 = part1Algo <$> input

part2 :: IO Integer
part2 = part2Algo <$> input

input :: IO String
input = readFile "src/input_day2.txt"

part1Algo :: String -> Integer
part1Algo = part1Checksum . parseIntegers

part2Algo :: String -> Integer
part2Algo = part2Checksum . parseIntegers

parseIntegers :: String -> [[Integer]]
parseIntegers = map (map read . words) . lines

part1Checksum :: [[Integer]] -> Integer
part1Checksum = sum . map rowPart1Checksum

rowPart1Checksum :: [Integer] -> Integer
rowPart1Checksum ls = maximum ls - minimum ls

part2Checksum :: [[Integer]] -> Integer
part2Checksum = sum . map evenlyDivisibleResult

evenlyDivisibleResult :: [Integer] -> Integer
evenlyDivisibleResult = fromJust . foldr helper Nothing . allPairs
  where
    allPairs ls = [(x,y) | x <- ls, y <- ls, x < y]
    -- allPairs ls = [(x,y) | (x:ys) <- tails ls, y <- ys]
    helper (x,y) acc = if isJust acc
                         then acc
                         else resultIfEvenDivide x y

    -- TODO if I could insure that n1 was always greater than n2 then I could avoid the greater/lesser logic
    resultIfEvenDivide :: Integer -> Integer -> Maybe Integer
    resultIfEvenDivide n1 n2 = let greater = max n1 n2
                                   lesser = min n1 n2
                              in if greater `mod` lesser == 0
                                   then Just (greater `div` lesser)
                                   else Nothing

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ parseIntegersTests
  , rowPart1ChecksumTests
  , part1ChecksumTests
  , evenlyDivisibleResultTests
  , part2AlgoTests
  , resultTests
  ]
  where
    parseIntegersTests = testGroup "parseIntegers"
      [ testCase "5 1 9 5\n\
                 \7 5 3\n\
                 \2 4 6 8" $ parseIntegers "5 1 9 5\n\
                                          \7 5 3\n\
                                          \2 4 6 8" @?= [ [5,1,9,5]
                                                        , [7,5,3]
                                                        , [2,4,6,8]] ]
    rowPart1ChecksumTests = testGroup "rowChecksum"
      [ testCase "[1,4]" $ rowPart1Checksum [1,4] @?= 3
      , testCase "[1,1,2,2,2,4,100]" $ rowPart1Checksum [1,1,2,2,2,4,100] @?= 99 ]
    part1ChecksumTests = testGroup "checksum"
      [ testCase "[[5,1,9,5],[7,5,3],[2,4,6,8]]" $ part1Checksum [[5,1,9,5],[7,5,3],[2,4,6,8]] @?= 18 ]
    evenlyDivisibleResultTests = testGroup "evenlyDivisibleResult"
      [ testCase "[5,9,2,8]" $ evenlyDivisibleResult [5,9,2,8] @?= 4 ]
    part2AlgoTests = testGroup "result2Algo"
      [ testCase "5 9 2 8\n\
                 \9 4 7 3\n\
                 \3 8 6 5" $ part2Algo "5 9 2 8\n\
                                         \9 4 7 3\n\
                                         \3 8 6 5" @?= 9 ]
    resultTests = testGroup "results from input"
      [ testCase "part 1" $ input >>= \i -> part1Algo i @?= 44670
      , testCase "part 2" $ input >>= \i -> part2Algo i @?= 285 ]
