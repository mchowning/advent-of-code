module DayData where

type Result = String

data Day =
  Day { part1 :: Result
      , part2 :: Result }
      deriving (Show)
