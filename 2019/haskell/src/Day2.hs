{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Util

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Vector.Unboxed (Vector, (//), (!))
import qualified Data.Vector.Unboxed as V

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' = V.head . runProgram . updateInput 12 2

runProgram :: Vector Int -> Vector Int
runProgram = runProgram' 0
  where
    runProgram' :: Int -> Vector Int -> Vector Int
    runProgram' n ls =
      let (a:b:c:d:_) = V.toList (V.slice n 4 ls)
      in if a == 99
           then ls
           else let
               op = if a == 1 then (+) else (*)
               newVal = op (ls ! b) (ls ! c)
               newLs = ls // [(d,newVal)]
             in runProgram' (n + 4) newLs


updateInput :: Int -> Int -> Vector Int -> Vector Int
updateInput a b = (// [(1,a), (2,b)])

--------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: Vector Int -> Int
part2' = nounVerbHash . findResult 19690720 candidates
  where
    minNum = 0
    maxNum = 99
    candidates = [(x,y) | x <- [minNum..maxNum], y <- [minNum..maxNum]]
    nounVerbHash (a,b) = 100 * a + b

findResult :: Int -> [(Int, Int)] -> Vector Int -> (Int, Int)
findResult expected ((x,y) : xys) ls =
  let updatedList = updateInput x y ls
      actual = V.head (runProgram updatedList)
  in if actual == expected
        then (x,y)
        else findResult expected xys ls

--------------------------------------------------------------

readInput :: IO (Vector Int)
readInput = V.fromList <$> parseInput (decimal `sepBy1` char ',') "day2.txt"