{-# OPTIONS_GHC -Wall -Werror #-}

module Day17 where

import Data.Function(on)
import Data.List
import Test.HUnit

input :: [Int]
input = [33,14,18,20,45,35,16,35,1,13,18,13,50,44,48,6,24,41,30,42]

result1 :: Int
result1 = length $ subsequencesThatTotal 150 input

result2 :: Int
result2 = length . head . sortAndGroupOn length $ subsequencesThatTotal 150 input

sortAndGroupOn :: (Foldable t, Eq b) => (t a -> b) -> [t a] -> [[t a]]
sortAndGroupOn f = groupBy ((==) `on` f) . sortOn length

subsequencesThatTotal :: Int -> [Int] -> [[Int]]
subsequencesThatTotal n = filter ((== n) . sum) . subsequences


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ result1 ~?= 1304
  , result2 ~?= 18
  ]
