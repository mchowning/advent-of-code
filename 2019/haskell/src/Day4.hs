module Day4 where

import Data.Char (digitToInt)
import Data.List

part1 :: Int
part1 = length (filter passwordSatisfiesPart1Rule range)

-- 589 too low
part2 :: Int
part2 = length (filter passwordSatisfiesPart2Rule range)

passwordSatisfiesPart1Rule :: Int -> Bool
passwordSatisfiesPart1Rule n = neverDecreases (toDigits n) && hasAtLeastTwoAdjacent n

passwordSatisfiesPart2Rule :: Int -> Bool
passwordSatisfiesPart2Rule n = neverDecreases (toDigits n) && hasTwoAdjacent n

range :: [Int]
range = [fst input..snd input]

hasTwoAdjacent :: Int -> Bool
hasTwoAdjacent = elem 2 . map length . group . show

hasAtLeastTwoAdjacent :: Int -> Bool
hasAtLeastTwoAdjacent = any (> 1) . map length . group . show

neverDecreases :: [Int] -> Bool
neverDecreases [] = True
neverDecreases [_] = True
neverDecreases (x:y:zs) = x <= y && neverDecreases (y:zs)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

input :: (Int, Int)
input = (307237, 769058)
