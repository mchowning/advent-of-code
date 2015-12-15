{-# OPTIONS_GHC -Wall -Werror #-}

module Day14 where

import Data.List
import Test.HUnit

data Reindeer = Reindeer { flySpeed :: Int
                         , flyTime :: Int
                         , restTime :: Int
                         } deriving (Show, Eq)

result1 :: Int
result1 = maxPositionAtTime 2503 input

result2 :: Int
result2 = maxScoreAtTime 2503 input

maxScoreAtTime :: Int -> String -> Int
maxScoreAtTime totalT = maximum . calculateScoresAtTime totalT . parseLines

calculateScoresAtTime :: Int -> [Reindeer] -> [Int]
calculateScoresAtTime totalT = numTimesIsInLead . map (positionsAtAllTimes totalT) 
  where
    positionsAtAllTimes :: Int -> Reindeer -> [Int]
    positionsAtAllTimes t r = map (`positionAtTime` r) [1..t] 

    numTimesIsInLead :: [[Int]] -> [Int]
    numTimesIsInLead = map (length . filter id) . isInLead
      where
        isInLead :: [[Int]] -> [[Bool]]
        isInLead = transpose . map isMax . transpose

        isMax :: [Int] -> [Bool]
        isMax ls = map (maximum ls ==) ls


maxPositionAtTime :: Int -> String -> Int
maxPositionAtTime t = maximum . map (positionAtTime t) . parseLines

positionAtTime :: Int -> Reindeer -> Int
positionAtTime remainingT r@(Reindeer flyS flyT restT)
  | remainingT <= flyT         = flyS * remainingT
  | remainingT <= flyT + restT = flyS * flyT
  | otherwise                  = flyT * flyS + positionAtTime (remainingT - flyT - restT) r

parseLines :: String -> [Reindeer]
parseLines = map parseLine . lines
  where
    parseLine :: String -> Reindeer
    parseLine str = let [_,_,_,flySp,_,_,flyT,_,_,_,_,_,_,restT,_] = words str 
                    in Reindeer (read flySp) (read flyT) (read restT)
    --parseLine str = let lineWords = words str 
                        --flySp = read (lineWords !! 3)
                        --flyT = read (lineWords !! 6)
                        --restT = read (lineWords !! 13)
                    --in Reindeer flySp flyT restT

input :: String
input = "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.\n\
        \Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.\n\
        \Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.\n\
        \Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.\n\
        \Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.\n\
        \Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.\n\
        \Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.\n\
        \Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.\n\
        \Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds."
        

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseLines (unlines [comet,dancer]) ~?= [ Reindeer 14 10 127
                                            , Reindeer 16 11 162 ]

  , positionAtTime 15  (Reindeer 20 10 100) ~?= 200
  , positionAtTime 5   (Reindeer 20 10 100) ~?= 100
  , positionAtTime 100 (Reindeer 20 10 100) ~?= 200
  , positionAtTime 200 (Reindeer 20 10 100) ~?= 400

  , maxPositionAtTime 10 (unlines [comet,dancer]) ~?= 160

  , maxScoreAtTime 1000 (unlines [comet,dancer])  ~?= 689

  , result1 ~?= 2640
  , result2 ~?= 1102
  ]
    --where
comet :: String
comet = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
dancer :: String
dancer = "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

