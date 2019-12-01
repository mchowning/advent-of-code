{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import Util
import TestHelpers

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [Int] -> Int
part1' = sum . fmap calculateFuel

calculateFuel :: Int -> Int
calculateFuel = subtract 2 . (`div` 3)

-- calculateFuel n = (n `div` 3) - 2

--------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: [Int] -> Int
part2' = sum . fmap calculateFuelRecursive

calculateFuelRecursive :: Int -> Int
calculateFuelRecursive =
  sum . takeWhile (> 0) . tail . iterate calculateFuel

-- calculateFuelRecursive n =
--   let more = calculateFuel n
--   in if more > 0
--        then more + calculateFuelRecursive more
--        else 0


--------------------------------------------------------------

readInput :: IO [Int]
readInput = parseInput (decimal `sepBy1` eol) "day1.txt"

-- readInput = fmap read . lines <$> readFile "../inputs/day1.txt"

test :: IO ()
test = runTestCases (TestCase "part 1" 3299598 part1)
                    (TestCase "part 2" 4946546 part2)