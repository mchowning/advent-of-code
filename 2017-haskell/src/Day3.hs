{-# LANGUAGE LambdaCase #-}

module Day3 where

import qualified Data.Map.Lazy    as M
-- import           Debug.Trace      (trace)
import           Test.Tasty
import           Test.Tasty.HUnit

import           DayData

type Level = Int
type Distance = Integer
type RemainingNum = Integer
type TravelValue = Either Distance RemainingNum

result :: IO Day
result = return $ Day (show $ part1Algo input) (show $ part2Algo input)

input :: Integer
input = 277678

part1Algo :: Integer -> Distance
part1Algo 1 = 0
part1Algo n = fromLeft $ helper (n-1) 1
  where
    fromLeft :: TravelValue -> Distance
    fromLeft (Left a) = a
    fromLeft _        = undefined

    helper :: RemainingNum -> Level -> TravelValue
    helper rn level =
      case processLevel level (Right rn) of
        (Left a)  -> Left (a + toInteger level)
        (Right b) -> helper b (level+1)

processLevel :: Level -> TravelValue -> TravelValue
processLevel _ tv@(Left _) = tv
processLevel level tv = foldr (\_ tv' -> processSide level tv') tv ([1..4] :: [Int])

processSide :: Level -> TravelValue -> TravelValue
processSide _ tv@(Left _) = tv
processSide level (Right number) =
  let sLength = sideLength level
  in if number > sLength
       then Right $ number - sLength
       else Left $ abs(toInteger number - sLength `div` 2)

sideLength :: Level -> Integer
sideLength = (*2) . toInteger

-------------------------------------------------------------

data Direction = East | North | West | South deriving (Eq, Show, Ord)
type Coordinate = (Integer, Integer)
data Position = Position Coordinate Direction deriving (Eq, Show, Ord)
type Grid = M.Map Coordinate Integer

turnLeft :: Direction -> Direction
turnLeft = \case
  East -> North
  North -> West
  West -> South
  South -> East

part2Algo :: Integer -> Integer
part2Algo n = let startGrid = M.singleton (0,0) 1
                  startPos = Position (0,0) East
              in move n startGrid startPos

move :: Integer -> Grid -> Position -> Integer
move n grid position =
  let (firstPrefPos@(Position firstPrefCoord _), secondPrefPos) = getPossibleMoves position
      newPosition@(Position newCoords _) = if not (M.member firstPrefCoord grid) then firstPrefPos else secondPrefPos
      valueForNextPosition = getValueForPosition newCoords grid
      newGrid = M.insert newCoords valueForNextPosition grid
  in if valueForNextPosition > n
       then valueForNextPosition
       else move n newGrid newPosition

getValueForPosition :: Coordinate -> Grid -> Integer
getValueForPosition (x,y) grid =
  sum $ map (flip (M.findWithDefault 0) grid) [ (x+1, y-1)
                                               , (x+1, y)
                                               , (x+1, y+1)
                                               , (x, y-1)
                                               , (x, y+1)
                                               , (x-1, y-1)
                                               , (x-1, y)
                                               , (x-1, y+1) ]

getPossibleMoves :: Position -> (Position, Position)
getPossibleMoves (Position (x,y) direction) =
  case direction of
    East  -> (Position (x, y+1) North, Position (x+1, y) East)
    North -> (Position (x-1, y) West,  Position (x, y+1) North)
    West  -> (Position (x, y-1) South, Position (x-1, y) West)
    South -> (Position (x+1, y) East,  Position (x, y-1) South)

-------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ sideLengthTests
  , processSideTests
  , processLevelTests
  , part1AlgoTests
  , part2AlgoTests
  ]
  where
    sideLengthTests = testGroup "sideLength"
      [ testCase "1" $ sideLength 1 @?= 2
      , testCase "2" $ sideLength 2 @?= 4
      , testCase "3" $ sideLength 3 @?= 6
      ]
    processSideTests = testGroup "processSide"
      [ testCase "any Left value" $ processSide 12 (Left 3) @?= Left 3 -- quickcheck would be better
      -- , testCase "length 1, pos 0" $ processSide 1 (Right 0) @?= Right 0
      , testCase "level 1, pos 1" $ processSide 1 (Right 1) @?= Left 0
      , testCase "level 1, pos 2" $ processSide 1 (Right 2) @?= Left 1
      , testCase "level 1, pos 100" $ processSide 1 (Right 100) @?= Right 98
      , testCase "level 2, pos 1" $ processSide 2 (Right 1) @?= Left 1
      , testCase "level 2, pos 2" $ processSide 2 (Right 2) @?= Left 0
      , testCase "level 2, pos 3" $ processSide 2 (Right 3) @?= Left 1
      , testCase "level 2, pos 4" $ processSide 2 (Right 4) @?= Left 2
      , testCase "level 2, pos 5" $ processSide 2 (Right 5) @?= Right 1
      , testCase "level 3, pos 1" $ processSide 3 (Right 1) @?= Left 2
      , testCase "level 3, pos 2" $ processSide 3 (Right 2) @?= Left 1
      , testCase "level 3, pos 3" $ processSide 3 (Right 3) @?= Left 0
      , testCase "level 3, pos 4" $ processSide 3 (Right 4) @?= Left 1
      , testCase "level 3, pos 5" $ processSide 3 (Right 5) @?= Left 2
      , testCase "level 3, pos 6" $ processSide 3 (Right 6) @?= Left 3
      , testCase "level 3, pos 7" $ processSide 3 (Right 7) @?= Right 1
      ]
    processLevelTests = testGroup "processLevel"
      [ testCase "any Left value" $ processLevel 1 (Left 9) @?= Left 9 -- quickcheck would be better
      , testCase "level 1, remaining 1" $ processLevel 1 (Right 1) @?= Left 0

      , testCase "level 1, remaining 2" $ processLevel 1 (Right 2) @?= Left 1
      , testCase "level 1, remaining 3" $ processLevel 1 (Right 3) @?= Left 0
      , testCase "level 1, remaining 4" $ processLevel 1 (Right 4) @?= Left 1
      , testCase "level 1, remaining 5" $ processLevel 1 (Right 5) @?= Left 0
      , testCase "level 1, remaining 6" $ processLevel 1 (Right 6) @?= Left 1
      , testCase "level 1, remaining 7" $ processLevel 1 (Right 7) @?= Left 0
      , testCase "level 1, remaining 8" $ processLevel 1 (Right 8) @?= Left 1
      , testCase "level 1, remaining 9" $ processLevel 1 (Right 9) @?= Right 1
      , testCase "level 1, remaining 100" $ processLevel 1 (Right 100) @?= Right 92
      ]
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
