module DayData where

type Result = String

data DayResult =
  DayResult { part1 :: Result
            , part2 :: Result }
            deriving (Show)
