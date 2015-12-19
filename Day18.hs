{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Day18 where

import Test.HUnit
import Data.Array.Repa(Z(..),(:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Undefined as RU
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Text.Printf


-- input is 100x100 character string vector with newline characters for each new row
results :: IO ()
results = do input <- readFile "day18_input.txt"
             printResult 1 $ result1 input
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

--2241 too high
result1 :: String -> Int
result1 = countLights . cycleWorldNTimes 100 . getWorld . initializeInput . lines

countLights :: R.Array R.U R.DIM2 Int -> Int
countLights = sum . R.toList

cycleWorldNTimes :: Int -> R.Array R.U R.DIM2 Int -> R.Array R.U R.DIM2 Int
cycleWorldNTimes n = head . drop n . iterate cycleWorld

cycleWorld :: R.Array R.U R.DIM2 Int -> R.Array R.U R.DIM2 Int
cycleWorld i = updateWorldState i (countNeighbors i) 

-- On is 1 and off is 0
initializeInput :: [String] -> [[Int]]
initializeInput = map (map getLight)
  where getLight :: Char -> Int
        getLight '.' = 0
        getLight '#' = 1
        getLight _   = undefined

neighborStencil :: Stencil R.DIM2 Int
neighborStencil = [stencil2| 1 1 1
                             1 0 1
                             1 1 1 |]

world :: R.Array R.U R.DIM2 Int 
world = R.fromListUnboxed (Z :. 3 :. 3) [ 1, 0, 1
                                        , 0, 0, 1
                                        , 0, 0, 0 ]

-- requires that all lists inside the list of Bools and the parent list all be of equal length
getWorld :: [[Int]] -> R.Array R.U R.DIM2 Int
getWorld ls = getWorld' (length ls) (concat ls)

getWorld' :: Int -> [Int] -> R.Array R.U R.DIM2 Int
getWorld' len = R.fromListUnboxed (Z :. len :. len) 

countNeighbors :: R.Array R.U R.DIM2 Int -> RU.Array PC5 R.DIM2 Int
countNeighbors = mapStencil2 (BoundConst 0) neighborStencil

-- R.computeUnboxedS only serves to change the Array from R.D (delayed) to R.U (unboxed)
updateWorldState :: R.Array R.U R.DIM2 Int -> RU.Array PC5 R.DIM2 Int -> R.Array R.U R.DIM2 Int
updateWorldState worldState = R.computeUnboxedS . R.zipWith updateLightState worldState

-- 1 is on and 0 is off
updateLightState :: Int -> Int -> Int
updateLightState 1 n
  | n `elem` [2,3] = 1
  | otherwise      = 0
updateLightState 0 n
  | n == 3 = 1
  | otherwise = 0
updateLightState _ _ = undefined


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ initializeInput (lines "#\n.") ~?= [[1],[0]]

  , R.toList (getWorld testArray) ~?= concat testArray

  , R.toList (countNeighbors $ getWorld testArray) ~?= [ 1, 2, 0
                                                       , 3, 5, 2
                                                       , 2, 3, 1 ]

  , countLights (getWorld testArray) ~?= 5

  , let worldState1 = getWorld testArray
        neighborCount = countNeighbors worldState1
        worldState2 = updateWorldState worldState1 neighborCount
    in R.toList worldState2 ~?= [ 0, 0, 0 
                                , 1, 0, 0 
                                , 1, 1, 0 ]

  , let worldState1 = getWorld testArray
        worldState2 = cycleWorld worldState1
    in R.toList worldState2 ~?= [ 0, 0, 0 
                                , 1, 0, 0 
                                , 1, 1, 0 ]

  , let worldState1 = getWorld testArray
        worldState2 = cycleWorld worldState1 
        worldState3 = cycleWorld worldState2 
    in R.toList worldState3 ~?= [ 0, 0, 0 
                                , 1, 1, 0
                                , 1, 1, 0 ]

  , let worldState1 = getWorld testArray
        worldState3 = cycleWorldNTimes 2 worldState1
    in R.toList worldState3 ~?= [ 0, 0, 0 
                                , 1, 1, 0
                                , 1, 1, 0 ]

  , let result = R.toList . cycleWorldNTimes 100 . getWorld . initializeInput . lines $ testInput 
    in result ~?= [ 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0
                  , 0, 0, 1, 1, 0, 0
                  , 0, 0, 1, 1, 0, 0
                  , 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0 ]

  ]
    where 
      testArray :: [[Int]]
      testArray = [[ 1, 0, 0 ]
                  ,[ 1, 0, 0 ]
                  ,[ 1, 1, 1 ]]
      testInput :: String
      testInput = unlines [".#.#.#","...##.","#....#","..#...","#.#..#","####.."]

