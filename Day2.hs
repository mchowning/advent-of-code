{-# OPTIONS_GHC -Wall -Werror #-}

module Day2( results
           , tests
           ) where 

import Data.List
import Data.List.Split
import Test.HUnit
import Text.Printf

results :: IO ()
results = do input <- readFile "day2_input.txt"
             printResult 1 $ requiredAmount paperF input  -- 1606483
             printResult 2 $ requiredAmount ribbonF input -- 3842356
  where 
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"


requiredAmount :: ([Int] -> Int) -> String -> Int
requiredAmount f = sum . map (f . parseDimens) . lines

parseDimens :: String -> [Int]
parseDimens = map read . splitOn "x"

ribbonF :: [Int] -> Int
ribbonF = (+) <$> ribbonAround <*> ribbonBow
  where
    ribbonAround :: [Int] -> Int
    ribbonAround = (*2) . sum . take 2 . sort

    ribbonBow :: [Int] -> Int
    ribbonBow = product

paperF :: [Int] -> Int
paperF = (+) <$> surfaceArea <*> slack
  where
    surfaceArea :: [Int] -> Int
    surfaceArea = sum . map ((*2) . product) . combinationsOf2

    slack :: [Int] -> Int
    slack = minimum . map product . combinationsOf2

combinationsOf2 :: [a] -> [[a]]
combinationsOf2 = combinationsOf 2
  where
    combinationsOf :: Int -> [a] -> [[a]]
    combinationsOf n = filter ((==n) . length) . subsequences

-- Tuple implementations

-- Simple implementation that only handles lists of 3
-- combinations :: [a] -> [(a,a)]
-- combinations [x,y,z] = [(x,y),(x,z),(y,z)]
-- combinations _       = undefined


-- More advanced implementations that handle lists of any length

-- explicit recursion
-- combinations ls@(_:xs) = combinations' ls ++ combinations xs
--   where
--     combinations' :: [a] -> [(a,a)]
--     combinations' (y:ys) = (,) <$> [y] <*> ys
--     combinations' _      = []
-- combinations _ = []

-- handling recursion with fold
-- combinations = foldr accumulateCombosWithFirst [] . tails
--   where
--     accumulateCombosWithFirst :: [a] -> [(a,a)] -> [(a,a)]
--     accumulateCombosWithFirst ls acc = combosWithFirst ls ++ acc
--
--     combosWithFirst :: [a] -> [(a,a)]
--     combosWithFirst (x:xs) = (,) <$> pure x <*> xs
--     combosWithFirst _ = []

tests :: IO Counts
tests = runTestTT $ TestList [ requiredAmount paperF "2x3x4\n1x1x10" ~?= 58+43
                             , requiredAmount ribbonF "2x3x4\n1x1x10" ~?= 34+14

                             , parseDimens "45x222x53" ~?= [45,222,53]
                             , parseDimens "1x1x1" ~?= [1,1,1]

                             , paperF [2,3,4] ~?= 58
                             , paperF [1,1,10] ~?= 43

                             , ribbonF [2,3,4] ~?= 34
                             , ribbonF [1,1,10] ~?= 14

                             , length (combinationsOf2  "abc") ~?= 3
--                              , testCombinationContains [1,2,3] (1,2)
--                              , testCombinationContains [1,2,3] (1,3)
--                              , testCombinationContains [1,2,3] (2,3)
                             , testCombinationOf2Contains [1,2,3] [1,2]
                             , testCombinationOf2Contains [1,2,3] [1,3]
                             , testCombinationOf2Contains [1,2,3] [2,3]
                             ]
  where
    testCombinationOf2Contains :: [Int] -> [Int] -> Test
    testCombinationOf2Contains input expected = TestCase(assertBool "" (elem expected $ combinationsOf2 input))

