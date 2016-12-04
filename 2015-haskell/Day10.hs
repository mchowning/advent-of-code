{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Test.HUnit

import qualified Data.ByteString.Char8 as B

-- import Data.Char
-- import Data.List

-- main :: IO ()
-- main = print result2

input :: Integer
input = 1113122113

{-
  difference in compiled runtime:
  ByteString:  3.6s
  String:     22.0s
-}

--- ByteString implementation

-- 360154
result1 :: Int
result1 = B.length $ lookSay 40 input

-- 5103798
result2 :: Int
result2 = B.length $ lookSay 50 input

lookSay :: Int -> Integer -> B.ByteString
lookSay n = last . take (n+1) . iterate lookSay' . B.pack . show

lookSay' :: B.ByteString -> B.ByteString
lookSay' = B.concat . map helper . B.group

helper :: B.ByteString -> B.ByteString
helper x = let num = B.pack . show $ B.length x
               char = B.head x
           in num `B.snoc` char

--- Just using Ints and Chars
-- this implementation can't run in ghci

-- result1 :: Int
-- result1 = lengthOfResult $ lookSay 40 input

-- result2 :: Int
-- result2 = lengthOfResult $ lookSay 50 input
--
-- lengthOfResult :: Integer -> Int
-- lengthOfResult = length . show
--
-- lookSay :: Int -> Integer -> Integer
-- lookSay n = last . take (n+1) . iterate lookSay'
--   where
--     lookSay' :: Integer -> Integer
--     lookSay' = read . map intToDigit . process . map digitToInt . show
--
--     process :: [Int] -> [Int]
--     process = concatMap (\xs -> [length xs, head xs]) . group


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList
  [ lookSay 1 1 ~?= "11"
  , lookSay 2 1 ~?= "21"
  , lookSay 3 1 ~?= "1211"
  , lookSay 4 1 ~?= "111221"
  , lookSay 5 1 ~?= "312211"
  ]
