{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import           Util
import qualified Day5

import Data.List (foldl', permutations)
import           Data.Text                  (Text)
import           Data.Vector.Unboxed        (Vector, (!), (//))
import qualified Data.Vector.Unboxed        as V

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import Debug.Trace

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' program = maximum (runAllAmps program <$> possibilities)

possibilities :: [[Int]]
possibilities = permutations [0..4]

runAllAmps :: Vector Int -> [Int] -> Int
runAllAmps program = foldl' (\acc i -> head (runProgram [i, acc] program)) 0

runProgram :: [Int] -> Vector Int -> [Int]
runProgram = Day5.runProgram True

readInput :: IO (Vector Int)
readInput = Day5.readInputFrom "day7.txt"
