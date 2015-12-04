{-# OPTIONS_GHC -Wall -Werror #-}

module Day2( results
           , tests
           ) where 

import Control.Monad
import Data.List
import Data.List.Split
import Test.HUnit
import Text.Printf

results :: IO ()
results = do input <- readFile "day2_input.txt"
             printResult 1 $ requiredAmount paperF input
             printResult 2 $ requiredAmount ribbonF input
  where 
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"


requiredAmount :: ([Int] -> Int) -> String -> Int
requiredAmount f = sum . map (f . parseDimens) . lines

parseDimens :: String -> [Int]
parseDimens = map read . splitOn "x"

ribbonF :: [Int] -> Int
ribbonF = liftM2 (+) ribbonAround ribbonBow
  where
    ribbonAround :: [Int] -> Int
    ribbonAround = (*2) . sum . take 2 . sort

    ribbonBow :: [Int] -> Int
    ribbonBow = product

paperF :: [Int] -> Int
paperF = liftM2 (+) surfaceArea slack
  where
    surfaceArea :: [Int] -> Int
    surfaceArea = sum . map (\(x,y) -> 2*x*y) . combinations
    --surfaceArea = sum . map ((*2) . uncurry (*)) . combinations

    slack :: [Int] -> Int
    slack = minimum . map (uncurry (*)) . combinations
    --slack = minimum . map (\(x,y) -> x*y) . combinations

combinations :: [a] -> [(a,a)]
combinations [x,y,z] = [(x,y),(x,z),(y,z)]
combinations _       = undefined

tests :: IO Counts
tests = runTestTT $ TestList [ requiredAmount paperF "2x3x4\n1x1x10" ~?= 58+43
                             , requiredAmount ribbonF "2x3x4\n1x1x10" ~?= 34+14

                             , parseDimens "45x222x53" ~?= [45,222,53]
                             , parseDimens "1x1x1" ~?= [1,1,1]

                             , paperF [2,3,4] ~?= 58
                             , paperF [1,1,10] ~?= 43

                             , ribbonF [2,3,4] ~?= 34
                             , ribbonF [1,1,10] ~?= 14

                             , length (combinations "abc") ~?= 3
                             , testCombinationContains [1,2,3] (1,2)
                             , testCombinationContains [1,2,3] (1,3)
                             , testCombinationContains [1,2,3] (2,3)
                             ]
  where
    testCombinationContains :: [Int] -> (Int,Int) -> Test
    testCombinationContains input expected = TestCase(assertBool "" (elem expected $ combinations input))

