{-# OPTIONS_GHC -Wall -Werror #-}

module Day20 where

import Test.HUnit
import Data.List

import Criterion.Main

type Houses = [Int]
type Elf = (Int,Houses)


input :: Int
input = 36000000

-- 831600
result1 :: Int
result1 = (+1) . length . takeWhile (< input) $ foldr (\x acc -> (10 * sum (allFactors x)):acc) [] [1..]

-- 884520
result2 :: Int
result2 = (+1) . length . takeWhile (< input) $ foldr (\x acc -> (11 * sum (filter (\f -> f * 50 >= x) (allFactors x))):acc) [] [1..]

allFactors :: Int -> [Int]
allFactors n = nub . concat $ [[x, q] | x <- [1..intSqrt n], let (q, r) = divMod n x, r == 0]

-- amazingly slow
allFactors' :: Int -> [Int]
allFactors' n = [ x | x <- [1..n], n `mod` x == 0]

tester :: Int
tester = fst . head . dropWhile ((< input) . snd) . iterate (callCounter (+1)) $ (0,0)

callCounter :: (Int -> Int) -> (Int,Int) -> (Int,Int)
callCounter f (n,_) = let numDeliveries = 10 * sum (allFactors (f n)) 
                      in (f n,numDeliveries)

intSqrt :: Int -> Int
intSqrt = floor . sqrt . (fromIntegral :: Int -> Double)

---------------------------------------------------------

benchmarks :: [Benchmark]
benchmarks = [ getBenches "allFactors" allFactors
             , getBenches "allFactors'" allFactors' ]

bench1 :: Benchmark
bench1 = bench "allFactors" $ whnf allFactors 1000000

bench2 :: Benchmark
bench2 = bench "factors" $ whnf allFactors' 1000000

getBenches :: String -> (Int -> [Int]) -> Benchmark
getBenches str f = bgroup str [ bench "100000" $ whnf f 100000
                              , bench "100001" $ whnf f 100001
                              , bench "100002" $ whnf f 100002 ]


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ allFactors 1 ~?= [1]
  , allFactors 2 \\ [1,2] ~?= []
  , allFactors 3 \\ [1,3] ~?= []
  , allFactors 4 \\ [1,2,4] ~?= []
  , allFactors 5 \\ [1,5] ~?= []
  , allFactors 6 \\ [1,2,3,6] ~?= []

  , allFactors' 1 ~?= [1]
  , allFactors' 2 \\ [1,2] ~?= []
  , allFactors' 3 \\ [1,3] ~?= []
  , allFactors' 4 \\ [1,2,4] ~?= []
  , allFactors' 5 \\ [1,5] ~?= []
  , allFactors' 6 \\ [1,2,3,6] ~?= []
  ]

--[>presents2 :: Int -> Int<]
--[>presents2 n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n<]
