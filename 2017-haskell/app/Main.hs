{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Lazy      as Map
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Turtle

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19

-- TODO pass time to run and use that to confirm running long parts?

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
runExercise day part =
  if part == 1
     then fst $ (Map.!) exercises day
     else snd $ (Map.!) exercises day

exercises :: Map.Map Int (IO String, IO String)
exercises = Map.fromList [ (1,  (return Day1.part1,         return Day1.part2))
                         , (2,  (show <$> Day2.part1,       show <$> Day2.part2))
                         , (3,  (return (show Day3.part1),  return (show Day3.part2)))
                         , (4,  (show <$> Day4.part1,       show <$> Day4.part2))
                         , (5,  (show <$> Day5.part1,       show <$> Day5.part2)) -- slow!
                         , (6,  (show <$> Day6.part1,       show <$> Day6.part2))
                         , (7,  (show <$> Day7.part1,       show <$> Day7.part2))
                         , (8,  (show <$> Day8.part1,       show <$> Day8.part2))
                         , (9,  (show <$> Day9.part1,       show <$> Day9.part2))
                         , (10, (show <$> Day10.part1,      Day10.part2))
                         , (11, (show <$> Day11.part1,      show <$> Day11.part2))
                         , (12, (show <$> Day12.part1,      show <$> Day12.part2))
                         , (13, (show <$> Day13.part1,      show <$> Day13.part2))
                         , (14, (return (show Day14.part1), return (show Day14.part2)))
                         , (15, (return (show Day15.part1), return (show Day15.part2)))
                         , (16, (Day16.part1,               Day16.part2))
                         , (17, (return (show Day17.part1), return (show Day17.part2)))
                         , (18, (show <$> Day18.part1,      show <$> Day18.part2))
                         , (19, (Day19.part1,               Day19.part2))
                         ]
