{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day1 where

import Util (parseInput)

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad (when)

readInput :: IO [Int]
readInput = parseInput (decimal `sepBy1` eol) "../inputs/1.txt"

day1Part1 :: IO Int
day1Part1 = day1Part1' <$> readInput

day1Part2 :: IO Int
day1Part2 = day1Part2' <$> readInput

---------------------------------------------------------------------------------

-- day1Part1' :: [Int] -> Int
-- day1Part1' xs = head [x * y | x <- xs, y <- xs, x + y == 2020]


-- day1Part2' :: [Int] -> Int
-- day1Part2' xs =  head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

---------------------------------------------------------------------------------

day1Part1' :: [Int] -> Int
day1Part1' = productIfSumEquals2020 2

day1Part2' :: [Int] -> Int
day1Part2' =  productIfSumEquals2020 3

type PairSize = Int

productIfSumEquals2020 :: PairSize -> [Int] -> Int
productIfSumEquals2020 ps = product . head . filter ((== 2020) . sum) . combinations ps
 
combinations :: PairSize -> [Int] -> [[Int]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 xs = pure <$> xs
combinations ps (x : xs) = 
    let
        combos = (x :) <$> combinations (ps - 1) xs
        rest = combinations ps xs
    in combos <> rest

---------------------------------------------------------------------------------

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]

testDay1Part1 = (== 252724) <$> day1Part1
testDay1Part2 = (== 276912720) <$> day1Part2
testDay1 = do
    print =<< testDay1Part1 
    print =<< testDay1Part2