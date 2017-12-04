module Day3 where

import qualified Data.Map.Lazy    as M
import qualified Data.Set         as S
-- import           Debug.Trace      (trace)
import           Test.Tasty
import           Test.Tasty.HUnit

import           DayData

type Coordinate = (Integer, Integer)
data Direction = East | North | West | South deriving (Eq, Show, Ord)
data Position = Position Coordinate Direction deriving (Eq, Show, Ord)

-------------------------------------------------------------

type Level = Int
type Distance = Integer
type RemainingNum = Integer
data TravelValue = Result Distance
                   | Remains RemainingNum
                   deriving (Eq, Show)


result :: IO Day
result = return $ Day (show $ part1Algo input) (show $ part2Algo input)

input :: Integer
input = 277678

-- This (more elegant imo) solution is slower

part1Algo :: Integer -> Distance
part1Algo n =
  let startPosition = Position (0,0) East
      startSet = S.singleton (0,0)
  in checkState startSet n startPosition

checkState :: S.Set Coordinate -> Integer -> Position -> Integer
checkState set goal pos@(Position (x,y) _) =
  if goal == 1
    then abs x + abs y
    else makeMove set goal pos

makeMove :: S.Set Coordinate -> Integer -> Position -> Integer
makeMove set goal pos@(Position coord _) =
  let nextSet = S.insert coord set
      nextPosition = getNextLeftSpiralPosition (\c -> not $ S.member c set) pos
  in checkState nextSet (goal-1) nextPosition


getSetWithPosition :: Position -> S.Set Coordinate -> S.Set Coordinate
getSetWithPosition (Position coord _) = S.insert coord

-- Less elegant, but faster

-- part1Algo :: Integer -> Distance
-- part1Algo 1 = 0
-- part1Algo n = fromLeft $ helper (n-1) 1
--   where
--     fromLeft :: TravelValue -> Distance
--     fromLeft (Result a) = a
--     fromLeft _        = undefined

--     helper :: RemainingNum -> Level -> TravelValue
--     helper rn level =
--       case processLevel level (Remains rn) of
--         (Result a)  -> Result (a + toInteger level)
--         (Remains b) -> helper b (level+1)

-- processLevel :: Level -> TravelValue -> TravelValue
-- processLevel _ tv@(Result _) = tv
-- processLevel level tv = iterate (processSide level) tv !! 4

-- processSide :: Level -> TravelValue -> TravelValue
-- processSide _ tv@(Result _) = tv
-- processSide level (Remains number) =
--   let sLength = sideLength level
--   in if number > sLength
--        then Remains $ number - sLength
--        else Result $ abs(toInteger number - sLength `div` 2)

-- sideLength :: Level -> Integer
-- sideLength = (*2) . toInteger

-------------------------------------------------------------

type SquareValue = Integer

type Grid = M.Map Coordinate SquareValue

part2Algo :: SquareValue -> SquareValue
part2Algo n = let startGrid = M.singleton (0,0) 1
                  startPos = Position (0,0) East
              in move n startGrid startPos

move :: SquareValue -> Grid -> Position -> SquareValue
move n grid position =
  let newPosition = getNextLeftSpiralPosition (isGridCoordinateAvailable grid) position
      valueForNewPosition = getValueForPosition newPosition grid
  in if valueForNewPosition > n
       then valueForNewPosition
       else let newGrid = getGridWithPosition newPosition valueForNewPosition grid
            in move n newGrid newPosition


getNextLeftSpiralPosition :: (Coordinate -> Bool) -> Position -> Position
getNextLeftSpiralPosition pred position = chooseMove pred (getPossibleLeftSpiralMoves position)
  where
    chooseMove :: (Coordinate -> Bool) -> (Position, Position) -> Position
    chooseMove pred (p1@(Position coord _), p2) = if pred coord then p1 else p2

isGridCoordinateAvailable :: Grid -> Coordinate -> Bool
isGridCoordinateAvailable grid coord = not (M.member coord grid)


getValueForPosition :: Position -> Grid -> SquareValue
getValueForPosition (Position (x,y) _) grid =
  sum $ map (flip (M.findWithDefault 0) grid) [ (x+1, y-1)      --  X X X
                                              , (x+1, y)        --  X O X
                                              , (x+1, y+1)      --  X X X
                                              , (x, y-1)
                                              , (x, y+1)
                                              , (x-1, y-1)
                                              , (x-1, y)
                                              , (x-1, y+1) ]


getGridWithPosition :: Position -> SquareValue -> Grid -> Grid
getGridWithPosition (Position coords _) = M.insert coords


getPossibleLeftSpiralMoves :: Position -> (Position, Position)
getPossibleLeftSpiralMoves (Position (x,y) direction) =
  case direction of
    East  -> (Position (x, y+1) North, Position (x+1, y) East)
    North -> (Position (x-1, y) West,  Position (x, y+1) North)
    West  -> (Position (x, y-1) South, Position (x-1, y) West)
    South -> (Position (x+1, y) East,  Position (x, y-1) South)

-------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ part1AlgoTests
  , part2AlgoTests
  -- , sideLengthTests
  -- , processSideTests
  -- , processLevelTests
  ]
  where
    sideLengthTests = testGroup "sideLength"
    --   [ testCase "1" $ sideLength 1 @?= 2
    --   , testCase "2" $ sideLength 2 @?= 4
    --   , testCase "3" $ sideLength 3 @?= 6
    --   ]
    -- processSideTests = testGroup "processSide"
    --   [ testCase "any Result value" $ processSide 12 (Result 3) @?= Result 3 -- quickcheck would be better
    --   -- , testCase "length 1, pos 0" $ processSide 1 (Remains 0) @?= Remains 0
    --   , testCase "level 1, pos 1" $ processSide 1 (Remains 1) @?= Result 0
    --   , testCase "level 1, pos 2" $ processSide 1 (Remains 2) @?= Result 1
    --   , testCase "level 1, pos 100" $ processSide 1 (Remains 100) @?= Remains 98
    --   , testCase "level 2, pos 1" $ processSide 2 (Remains 1) @?= Result 1
    --   , testCase "level 2, pos 2" $ processSide 2 (Remains 2) @?= Result 0
    --   , testCase "level 2, pos 3" $ processSide 2 (Remains 3) @?= Result 1
    --   , testCase "level 2, pos 4" $ processSide 2 (Remains 4) @?= Result 2
    --   , testCase "level 2, pos 5" $ processSide 2 (Remains 5) @?= Remains 1
    --   , testCase "level 3, pos 1" $ processSide 3 (Remains 1) @?= Result 2
    --   , testCase "level 3, pos 2" $ processSide 3 (Remains 2) @?= Result 1
    --   , testCase "level 3, pos 3" $ processSide 3 (Remains 3) @?= Result 0
    --   , testCase "level 3, pos 4" $ processSide 3 (Remains 4) @?= Result 1
    --   , testCase "level 3, pos 5" $ processSide 3 (Remains 5) @?= Result 2
    --   , testCase "level 3, pos 6" $ processSide 3 (Remains 6) @?= Result 3
    --   , testCase "level 3, pos 7" $ processSide 3 (Remains 7) @?= Remains 1
    --   ]
    -- processLevelTests = testGroup "processLevel"
    --   [ testCase "any Result value" $ processLevel 1 (Result 9) @?= Result 9 -- quickcheck would be better
    --   , testCase "level 1, remaining 1" $ processLevel 1 (Remains 1) @?= Result 0

    --   , testCase "level 1, remaining 2" $ processLevel 1 (Remains 2) @?= Result 1
    --   , testCase "level 1, remaining 3" $ processLevel 1 (Remains 3) @?= Result 0
    --   , testCase "level 1, remaining 4" $ processLevel 1 (Remains 4) @?= Result 1
    --   , testCase "level 1, remaining 5" $ processLevel 1 (Remains 5) @?= Result 0
    --   , testCase "level 1, remaining 6" $ processLevel 1 (Remains 6) @?= Result 1
    --   , testCase "level 1, remaining 7" $ processLevel 1 (Remains 7) @?= Result 0
    --   , testCase "level 1, remaining 8" $ processLevel 1 (Remains 8) @?= Result 1
    --   , testCase "level 1, remaining 9" $ processLevel 1 (Remains 9) @?= Remains 1
    --   , testCase "level 1, remaining 100" $ processLevel 1 (Remains 100) @?= Remains 92
    --   ]
    part1AlgoTests = testGroup "part1Algo"
      [ testCase "1" $ part1Algo 1 @?= 0
      , testCase "2" $ part1Algo 2 @?= 1
      , testCase "3" $ part1Algo 3 @?= 2
      , testCase "4" $ part1Algo 4 @?= 1
      , testCase "5" $ part1Algo 5 @?= 2
      , testCase "6" $ part1Algo 6 @?= 1
      , testCase "7" $ part1Algo 7 @?= 2
      , testCase "8" $ part1Algo 8 @?= 1
      , testCase "9" $ part1Algo 9 @?= 2
      , testCase "10" $ part1Algo 10 @?= 3
      , testCase "11" $ part1Algo 11 @?= 2
      , testCase "12" $ part1Algo 12 @?= 3
      , testCase "13" $ part1Algo 13 @?= 4
      , testCase "14" $ part1Algo 14 @?= 3
      , testCase "15" $ part1Algo 15 @?= 2
      , testCase "16" $ part1Algo 16 @?= 3
      , testCase "17" $ part1Algo 17 @?= 4
      , testCase "18" $ part1Algo 18 @?= 3
      , testCase "19" $ part1Algo 19 @?= 2
      , testCase "20" $ part1Algo 20 @?= 3
      , testCase "21" $ part1Algo 21 @?= 4
      , testCase "22" $ part1Algo 22 @?= 3
      , testCase "23" $ part1Algo 23 @?= 2
      , testCase "1024" $ part1Algo 1024 @?= 31
      , testCase "277678" $ part1Algo 277678 @?= 475 ]
    part2AlgoTests = testGroup "part2Algo"
      [ testCase "4" $ part2Algo 4 @?= 5
      , testCase "5" $ part2Algo 5 @?= 10
      , testCase "10" $ part2Algo 10 @?= 11
      , testCase "11" $ part2Algo 11 @?= 23
      , testCase "input" $ part2Algo input @?= 279138 ]
