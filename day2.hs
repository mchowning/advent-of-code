{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Monad
import Data.List
import Data.List.Split
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

    slack :: [Int] -> Int
    slack = minimum . map (\(x,y) -> x*y) . combinations

combinations :: [a] -> [(a,a)]
combinations [x,y,z] = [(x,y),(x,z),(y,z)]
combinations _       = undefined
