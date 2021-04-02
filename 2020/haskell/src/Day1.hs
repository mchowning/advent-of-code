{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day1 (part1, part2, part1', part2', combinations) where

import Util (parseInput)

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad (when)

readInput :: IO [Int]
readInput = parseInput (decimal `sepBy1` eol) "../inputs/1.txt"

part1 :: IO Int
part1 = part1' <$> readInput

part2 :: IO Int
part2 = part2' <$> readInput

---------------------------------------------------------------------------------

-- benchmark with test data: 
-- 19 ns ± 1.6 ns
-- 30 ns ± 1.7 ns
-- 22 ns ± 1.8 ns
part1' :: [Int] -> Int
part1' xs = head [x * y | x <- xs, y <- xs, x + y == 2020]

-- benchmark with test data: 
-- 562 ns ±  53 ns
-- 338 ns ±  13 ns
-- 332 ns ±  32 ns
part2' :: [Int] -> Int
part2' xs =  head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

---------------------------------------------------------------------------------

-- -- benchmark with test data: 
-- -- 114 ns ± 7.1 ns
-- -- 137 ns ± 3.3 ns
-- -- 116 ns ±  11 ns
-- day1Part1' :: [Int] -> Int
-- day1Part1' = productIfSumEquals2020 2

-- -- benchmark with test data: 
-- -- 596 ns ±  29 ns
-- -- 641 ns ± 8.6 ns
-- -- 567 ns ±  17 ns
-- day1Part2' :: [Int] -> Int
-- day1Part2' =  productIfSumEquals2020 3

type PairSize = Int

productIfSumEquals2020 :: PairSize -> [Int] -> Int
productIfSumEquals2020 ps = product . head . filter ((== 2020) . sum) . combinations ps
 
combinations :: PairSize -> [Int] -> [[Int]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 xs = (: []) <$> xs
combinations ps (x : xs) = 
    let combos = (x :) <$> combinations (ps - 1) xs
        rest = combinations ps xs
    in combos <> rest