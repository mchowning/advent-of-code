import Data.List

import Criterion.Main
import Debug.Trace


allFactors :: Int -> [Int]
allFactors n = nub . concat $ [[x, q] | x <- [1..intSqrt n], let (q, r) = divMod n x, r == 0]
{-allFactors n | trace (show n) True = nub . concat $ [[x, q] | x <- [1..intSqrt n], let (q, r) = divMod n x, r == 0]-}
{-allFactors _ = undefined-}

-- amazingly slow
allFactors' :: Int -> [Int]
allFactors' n = [ x | x <- [1..n], n `mod` x == 0]
{-allFactors' n | trace (show n) True  = [ x | x <- [1..n], n `mod` x == 0]-}
{-allFactors' _ = undefined-}

intSqrt :: Int -> Int
intSqrt = floor . sqrt . (fromIntegral :: Int -> Double)

main :: IO ()
{-main = print testFunction-}
main = defaultMain [ bench "allFactors" (whnf (testFunction allFactors) 1000)
                   , bench "allFactors'" (whnf (testFunction allFactors') 1000) ]

testFunction :: (Int -> [Int]) -> Int -> Int
testFunction f n = sum . concatMap f $ [1..n]

bench1 :: Benchmark
bench1 = bench "allFactors" $ whnf allFactors 1000000

bench2 :: Benchmark
bench2 = bench "factors" $ whnf allFactors' 1000000

getBenches :: String -> (Int -> [Int]) -> Benchmark
getBenches str f = bgroup str [ bench "1" $ whnf f 1
                              , bench "10" $ whnf f 10
                              , bench "100002" $ whnf f 100002 ]

