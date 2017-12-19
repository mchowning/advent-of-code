{-# LANGUAGE TupleSections, MultiWayIf #-}
module Day19 (part1, part2) where

import Data.Maybe (fromJust,fromMaybe,isJust,isNothing)
import qualified Data.Vector as V
import qualified Data.DList as D

import Test.Tasty
import Test.Tasty.HUnit
import Debug.Trace (trace)

data Direction = North
               | South
               | East
               | West
               deriving (Eq, Show)
type Coordinate = (Int, Int, Direction)

part1 :: IO String
part1 = D.toList . collectLettersOnPath <$> input

part2 :: IO Int
part2 = pathLength <$> input

type Graph = V.Vector (V.Vector Char)

input :: IO Graph
input = V.fromList . map V.fromList . lines <$> readFile "src/input_day19.txt"

collectLettersOnPath :: Graph -> D.DList Char
collectLettersOnPath g =
  let startPoint = (, 0, South) . fromJust . V.elemIndex '|' . V.head $ g
      (collected, _, _) = followPath g (D.empty, startPoint, 0)
  in collected
  
pathLength :: Graph -> Int
pathLength g =
  let startPoint = (, 0, South) . fromJust . V.elemIndex '|' . V.head $ g
      (_, _, n) = followPath g (D.empty, startPoint, 0)
  in n
  
-- FIXME just follow the path collecting the characters as I go
followPath :: Graph -> (D.DList Char, Coordinate, Int) -> (D.DList Char, Coordinate, Int)
followPath g (collected, c@(x, y, dir), n) =
  let mChar = getCharFor g c
  in if isNothing mChar || ((== ' ') . fromJust $ mChar)
       then (collected, (x, y, dir), n)
       else
         case fromJust mChar of
           e | e `elem` ['A' .. 'Z'] -> followPath g (D.snoc collected e, getNextCoord c, n+1)
           '+'                       -> followPath g (collected, getNewDirection g c, n+1)
           _                         -> followPath g (collected, getNextCoord c, n+1)
           
getNextCoord :: Coordinate -> Coordinate
getNextCoord (x,y,dir) = case dir of
  North -> (x, y-1, dir)
  South -> (x, y+1, dir)
  East   -> (x+1, y, dir)
  West  -> (x-1, y, dir)

getNewDirection :: Graph -> Coordinate -> Coordinate
getNewDirection g c@(x,y,d) = case d of
  North -> findEastWestDir c
  South -> findEastWestDir c
  West -> findNorthSouthDir c
  East -> findNorthSouthDir c
 where
  findEastWestDir (x', y', _) =
    if (/= Just ' ') (getCharFor g (x'-1, y', d))
      then (x'-1, y', West)
      else (x'+1, y', East)
  findNorthSouthDir (x', y', _) =
    if (/= Just ' ') (getCharFor g (x', y'-1, d))
      then (x', y'-1, North)
      else (x', y'+1, South)
  
getCharFor :: Graph -> Coordinate -> Maybe Char
getCharFor g (x,y,_) = g V.!? y >>= (V.!? x)


-------------------------------------------------------------------------------------

sampleInput :: Graph
sampleInput = V.fromList
  [ V.fromList "    |         "
  , V.fromList "    |  +--+   "
  , V.fromList "    A  |  C   "
  , V.fromList "F---|----E|--+"
  , V.fromList "    |  |  |  D"
  , V.fromList "    +B-+  +--+"
  ]
 
tests :: IO ()
tests = defaultMain $ testGroup "Day 19"
  [ testGroup "part 1"
    [ testCase "sample input" $ collectLettersOnPath sampleInput @?= D.fromList "ABCDEF"
    , testCase "real input" $ part1 >>= (@?= "MOABEUCWQS") ]
  , testGroup "part 2"
    [ testCase "sample input" $ pathLength sampleInput @?= 38
    , testCase "real input" $ part2 >>= (@?= 18058)
    ]
  ]