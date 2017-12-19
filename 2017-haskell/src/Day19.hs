{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
module Day19 (part1, part2) where

import           Data.Char        (isAlpha)
import qualified Data.DList       as D
import           Data.Maybe       (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Vector      as V

import           Test.Tasty
import           Test.Tasty.HUnit

data Direction = North
               | South
               | East
               | West
               deriving (Eq, Show)
type Coordinate = (Int, Int, Direction)

part1 :: IO String
part1 = lettersOnPath <$> input

part2 :: IO Int
part2 = pathLength <$> input

type Graph = V.Vector (V.Vector Char)

input :: IO Graph
input = V.fromList . map V.fromList . lines <$> readFile "src/input_day19.txt"

lettersOnPath :: Graph -> String
lettersOnPath g = filter isAlpha . D.toList . snd . followPath g $ (startCoordinate g, D.empty)

pathLength :: Graph -> Int
pathLength g = length . D.toList . snd $ followPath g (startCoordinate g, D.empty)

startCoordinate :: Graph -> Coordinate
startCoordinate = (, 0, South) . fromJust . V.elemIndex '|' . V.head

followPath :: Graph -> (Coordinate, D.DList Char) -> (Coordinate, D.DList Char)
followPath g (c@(x, y, dir), collected) =
  let mChar = getCharFor g c
--  in if isNothing mChar || ((== ' ') . fromJust $ mChar)
  in if isNothing mChar || mChar == Just ' '
       then ((x, y, dir), collected)
       else let thisChar = fromJust mChar
                nextCoord = case thisChar of
                              '+' -> getNewDirection g c
                              _   -> getNextCoord c
            in followPath g (nextCoord, D.snoc collected thisChar)

getNextCoord :: Coordinate -> Coordinate
getNextCoord (x,y,dir) = case dir of
  North -> (x, y-1, dir)
  South -> (x, y+1, dir)
  East  -> (x+1, y, dir)
  West  -> (x-1, y, dir)

getNewDirection :: Graph -> Coordinate -> Coordinate
getNewDirection g c@(_,_,d) = case d of
  North -> findEastWestDir c
  South -> findEastWestDir c
  West  -> findNorthSouthDir c
  East  -> findNorthSouthDir c
 where
  findEastWestDir (x,y,_) =
    if (/= Just ' ') (getCharFor g (x-1, y, d))
      then (x-1, y, West)
      else (x+1, y, East)
  findNorthSouthDir (x,y,_) =
    if (/= Just ' ') (getCharFor g (x, y-1, d))
      then (x, y-1, North)
      else (x, y+1, South)

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
    [ testCase "sample input" $ lettersOnPath sampleInput @?= "ABCDEF"
    , testCase "real input" $ part1 >>= (@?= "MOABEUCWQS") ]
  , testGroup "part 2"
    [ testCase "sample input" $ pathLength sampleInput @?= 38
    , testCase "real input" $ part2 >>= (@?= 18058)
    ]
  ]
