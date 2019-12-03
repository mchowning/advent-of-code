{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Util
import TestHelpers

import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, symbol)

import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as V

-- TODO: Update to use Vecotrs, mutable Vectors, Arrays for efficiency?

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [Int] -> Int
part1' = head . runProgram . updateInput 12 2

runProgram :: [Int] -> [Int]
runProgram = runProgram' 0
  where
    runProgram' :: Int -> [Int] -> [Int]
    runProgram' n ls =
      let (_, a:b:c:d:_) = splitAt n ls
      in if a == 99
           then ls
           else let
               op = if a == 1 then (+) else (*)
               newVal = op (ls !! b) (ls !! c)
               newLs = replaceElement d newVal ls
             in runProgram' (n + 4) newLs

-- updateInputForPart1 :: [Int] -> [Int]
-- -- updateInputForPart1 = replaceElement 1 12 . replaceElement 2 2
-- updateInputForPart1 = updateInput 12 2
-- updateInputForPart1 :: Vector Int -> Vector Int
-- updateInputForPart1 = (// [(1,12), (2,2)])


updateInput :: Int -> Int -> [Int] -> [Int]
updateInput a b = replaceElement 1 a . replaceElement 2 b

replaceElement :: Int -> Int -> [Int] -> [Int]
replaceElement i n xs =
  let (before, _ : after) = splitAt i xs
  in before ++ n : after

--------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: [Int] -> Int
part2' = nounVerbHash . findResult 19690720 candidates
  where
    minNum = 0
    maxNum = 99
    candidates = [(x,y) | x <- [minNum..maxNum], y <- [minNum..maxNum]]
    nounVerbHash (a,b) = 100 * a + b

findResult :: Int -> [(Int, Int)] -> [Int] -> (Int, Int)
findResult expected ((x,y) : xys) ls =
  let updatedList = updateInput x y ls
      actual = head (runProgram updatedList)
  in if actual == expected
        then (x,y)
        else findResult expected xys ls

--------------------------------------------------------------

readInput :: IO [Int]
readInput = parseInput (decimal `sepBy1` char ',') "day2.txt"
-- readInput :: IO (Vector Int)
-- readInput = V.fromList <$> parseInput (decimal `sepBy1` char ',') "day2.txt"

--------------------------------------------------------------


test :: IO ()
test = runTestCases (TestCase "part 1" 3101844 part1)
                    (TestCase "part 2" 8478 part2)
