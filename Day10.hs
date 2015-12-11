{-# OPTIONS_GHC -Wall -Werror #-}

module Day10 where

import Data.Char
import Data.List
import Test.HUnit

input :: Integer
input = 1113122113

{-main :: IO ()-}
{-main = print result2-}

-- Output blows up ghci, had to make an executable that would
-- run this to get the result
result2 :: Int
result2 = lengthOfResult 50 input

result1 :: Int
result1 = lengthOfResult 40 input

lengthOfResult :: Int -> Integer -> Int
lengthOfResult n = length . show . lookSay n

lookSay :: Int -> Integer -> Integer
lookSay n = last . take (n+1) . iterate lookSay'

lookSay' :: Integer -> Integer
lookSay' = read . map intToDigit . process . map digitToInt . show

process :: [Int] -> [Int]
process = concatMap (\xs -> [length xs, head xs]) . group

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ lookSay 1 1 ~?= 11
  , lookSay 2 1 ~?= 21
  , lookSay 3 1 ~?= 1211
  , lookSay 4 1 ~?= 111221
  , lookSay 5 1 ~?= 312211
  ]
