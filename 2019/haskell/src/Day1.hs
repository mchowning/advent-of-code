{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import Util

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [Int] -> Int
part1' = sum . fmap calculateFuel

calculateFuel :: Int -> Int
calculateFuel = subtract 2 . (`div` 3)

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
--   in if more <= 0
--        then 0
--        else more + calculateFuelRecursive more


--------------------------------------------------------------

readInput :: IO [Int]
readInput = parseInput (decimal `sepBy1` eol) "day1.txt"

-- readInput = fmap read . lines <$> readFile "../inputs/day1.txt"
