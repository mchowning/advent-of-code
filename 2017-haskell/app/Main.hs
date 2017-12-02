module Main where

import qualified Data.Map.Strict    as Map
import           Lib
import           System.Environment

import qualified Day1               as Day1
import           DayData

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "must pass the argument day"
    else let arg = read (head args)
         -- TODO use actual parser
         -- TODO handle the two exercises per day
         in if Map.member arg exercises
              -- TODO use String interpolation
              then putStrLn $ concat [ "Running the exercise for Day #"
                                     , show arg
                                     ,  " with result: "
                                     , part1 $ (Map.!) exercises arg
                                     ]
              else putStrLn $ "No exercises for Day #" ++ show arg

exercises :: Map.Map Integer DayResult
exercises = Map.fromList [ (1, Day1.result)]
