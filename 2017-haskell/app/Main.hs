{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Lazy      as Map
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Lib
import           System.Environment
import           Turtle
import           Turtle.Format

import qualified Day1
import qualified Day2
import           DayData

-- TODO parse a specific Part1/Part2 type from the second parameter?

main :: IO ()
main = do
  (day, part) <- options "Advent of Code 2017 Exercise script" parser
  if Map.member day exercises
    then let result = T.pack (runExercise day part)
         in TIO.putStrLn $ format ("Running "%d%"-"%d%", with result: "%s%"") day part result
    else TIO.putStrLn $ format ("No exercises for Day #"%d%"") day

parser :: Parser (Int, Int)
parser = (,) <$> argInt "day" "The day of the exercise "
             <*> argInt "part" "The part of the day's exercise to run"

runExercise :: Int -> Int -> String
runExercise day part = let partFunction = if part == 1 then part1 else part2
                       in partFunction $ (Map.!) exercises day

exercises :: Map.Map Int DayResult
exercises = Map.fromList [ (1, Day1.result)
                         , (2, Day2.result) ]
