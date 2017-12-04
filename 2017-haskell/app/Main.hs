{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE LambdaCase #-}
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
import qualified Day3
import           DayData

-- TODO parse a specific Part1/Part2 type from the second parameter?

data Error = InvalidDay | InvalidPart


main :: IO ()
main = do
  (day, part) <- options "Advent of Code 2017 Exercise script" parser
  if Map.member day exercises
    then do result <- runExercise day part
            TIO.putStrLn $ format ("Running "%d%"-"%d%", with result: "%s%"") day part (T.pack result)
    else TIO.putStrLn $ format ("No exercises for Day #"%d%"") day

parser :: Parser (Int, Int)
parser = (,) <$> argInt "day" "the day of an exercise: a number"
             <*> argInt "part" "the part of the day's exercise to run: the number 1 or 2"


runExercise :: Int -> Int -> IO String
runExercise day part = let partFunction = if part == 1 then part1 else part2
                       in do dayResult <- (Map.!) exercises day
                             return (partFunction dayResult)

exercises :: Map.Map Int (IO Day)
exercises = Map.fromList [ (1, Day1.result)
                         , (2, Day2.result)
                         , (3, Day3.result)
                         ]

-- getDay :: Int -> Either Error Day
-- getDay = \case
--   1 -> Right Day1.result
--   2 -> Right Day2.result
--   _ -> Left InvalidDay

-- getPartFunc :: Int -> Either Error (Day -> Result)
-- getPartFunc = \case
--   1 -> Right part1
--   2 -> Right part2
--   _ -> Left InvalidPart
