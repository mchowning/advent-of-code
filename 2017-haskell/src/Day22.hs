{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day22 (part1, part2) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import GHC.Generics

part1, part2 :: IO Int
part1 = numInfectedAfterNIterationsPart1 10000 <$> input
part2 = numInfectedAfterNIterationsPart2 10000000 <$> input

data Direction = North
               | East
               | South
               | West
               deriving (Eq, Show, Generic)
              
data NodeState = Clean
               | Weakened
               | Infected
               | Flagged
               deriving (Show, Eq, Ord)
               
type Pos = (Integer, Integer)
type Carrier = (Pos, Direction)
type Part1Grid = (S.Set Pos, Carrier, Int)

type Part2GridMap = M.Map Pos NodeState
type Part2Grid = (Part2GridMap, Carrier, Int)

input :: IO [String]
input = lines <$> readFile "src/input_day22.txt"

infectedNodes :: [String] -> S.Set Pos
infectedNodes ls =
  let indexed = concat $ zipWith (\row -> zipWith (\col c -> ((col, negate row), c)) [0..]) [0..] ls
  in S.fromList . map fst . filter ((== '#') . snd) $ indexed

startCarrier :: [String] -> Carrier
startCarrier ls =
  let width = toInteger . length . head $ ls
      height = toInteger . length $ ls
  in ((width `div` 2, negate (height `div` 2)), North)
  
numInfectedAfterNIterationsPart1 :: Int -> [String] -> Int
numInfectedAfterNIterationsPart1 iters s =
  let (_, _, n) = processNIterationsPart1 (infectedNodes s, startCarrier s, 0) iters
  in n

processNIterationsPart1 :: Part1Grid -> Int -> Part1Grid
processNIterationsPart1 g n = iterate updatePart1Grid g !! n

updatePart1Grid :: Part1Grid -> Part1Grid
updatePart1Grid = moveForwardPart1Grid . encounterPart1Node . turnCarrierPart1

turnCarrierPart1 :: Part1Grid -> Part1Grid
turnCarrierPart1 (s, (p,d), n) =
  let newD = if p `S.member` s then turnRight d else turnLeft d
  in (s, (p, newD), n)
  
encounterPart1Node :: Part1Grid -> Part1Grid
encounterPart1Node (s, c@(p,_), numCarrierInfected) =
  let isAlreadyInfected = p `S.member` s
      updateType = if isAlreadyInfected then S.delete else S.insert
      newNumCarrierInfected = if isAlreadyInfected then numCarrierInfected else 1+numCarrierInfected
  in (updateType p s, c, newNumCarrierInfected)
  
moveForwardPart1Grid :: Part1Grid -> Part1Grid
moveForwardPart1Grid (s, c, n) = (s, moveForward c, n)
  
moveForward :: Carrier -> Carrier
moveForward ((x,y), d) =
  (moveForwardPos, d)
 where
   moveForwardPos =
     case d of
       North -> (x  , y+1)
       East  -> (x+1, y  )
       South -> (x  , y-1)
       West  -> (x-1, y  )
       
numInfectedAfterNIterationsPart2 :: Int -> [String] -> Int
numInfectedAfterNIterationsPart2 n s =
--  let m =  M.fromList . zipWith (,Infected) . infectedNodes $ s; m :: M.Map Pos NodeState
  let m = M.fromSet (const Infected) . infectedNodes $ s
      (_, _, result) = (iterate updatePart2Grid (m, startCarrier s, 0) !! n)
  in result


updatePart2Grid :: Part2Grid -> Part2Grid
updatePart2Grid (m, c@(p, _), nInfected) =
  let oldState = M.findWithDefault Clean p m
      newCarrier = moveForward (turnCarrierPart2 oldState c)
      newMap = M.insert p (encounterPart2Node oldState) m
      newNInfected = if oldState == Weakened then 1+nInfected else nInfected
      
   in (newMap, newCarrier, newNInfected)
  
encounterPart2Node :: NodeState -> NodeState
encounterPart2Node Clean = Weakened
encounterPart2Node Weakened = Infected
encounterPart2Node Infected = Flagged
encounterPart2Node Flagged = Clean

turnCarrierPart2 :: NodeState -> Carrier -> Carrier
turnCarrierPart2 n (p, d) =
  let updateDir = case n of
                    Clean -> turnLeft
                    Weakened -> id
                    Infected -> turnRight
                    Flagged -> turnAround
  in (p, updateDir d)

turnAround :: Direction -> Direction
turnAround = turnRight . turnRight

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North


------------------------------------------------------------------------------------------------

sampleInput = ["..#", "#..", "..."]

instance Monad m => Serial m Direction

tests = defaultMain $ testGroup "Day 22"
  [ testCase "starting carrier position" $ startCarrier sampleInput @?= ((1,-1), North)
  , testCase "infected nodes" $ infectedNodes sampleInput @?= S.fromList [(2,0), (0,-1) ]
  
  , testGroup "part 1"
      [ testCase "sample input - 7 iterations" $ numInfectedAfterNIterationsPart1 7 sampleInput @?= 5
      , testCase "sample input - 70 iterations" $ numInfectedAfterNIterationsPart1 70 sampleInput @?= 41
      , testCase "sample input - 70 iterations" $ numInfectedAfterNIterationsPart1 10000 sampleInput @?= 5587
      , testCase "part 1" $ part1 >>= (@?= 5352)
      ]
  
  , testGroup "part 2"
      [ testCase "sample input - 1000 iterations" $ numInfectedAfterNIterationsPart2 100 sampleInput @?= 26
      -- , testCase "sample input - 10000000 iterations" $ numInfectedAfterNIterationsPart2 10000000  sampleInput @?= 2511944
      -- , testCase "part 2" $ part2 >>= (@?= 2511475)
      ]
  
  , testProperty "2 turns offset" $ \dir -> (turnLeft . turnRight $ dir) == dir
  , testProperty "4 turns offset" $ \dir -> (turnLeft .turnLeft . turnRight . turnRight $ dir) == dir
  ]
