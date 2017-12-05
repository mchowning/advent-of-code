module Day5 where

import           DayData

import           Test.Tasty
import           Test.Tasty.HUnit

-- Part 1 answer is 336905, this takes around 24s not compiled, 12s compiled

type Index = Int
data JumpState = JumpState Index [Int]
                 deriving (Eq, Show)

result :: IO Day
result =
  do part1Result <- (show . part1Algo) <$> input
     return (Day part1Result undefined)

input :: IO [Int]
input = (map read . lines) <$> readFile "src/input_day5.txt"

part1Algo :: [Int] -> Integer
part1Algo = countJumps . Just . JumpState 0

countJumps :: Maybe JumpState -> Integer
countJumps Nothing = 0
countJumps (Just js) = 1 + countJumps (performJump js)

performJump :: JumpState -> Maybe JumpState
performJump (JumpState index ls) =
  do newIndex <- getIndexForJumpFrom index ls
     newList <- incrementListAtIndex index ls
     return $ JumpState newIndex newList

incrementListAtIndex :: Index -> [Int] -> Maybe [Int]
incrementListAtIndex n ls
  | isValidListIndex n ls = let (before, element:after) = splitAt n ls
                            in Just $ before ++ (1 + element) : after
  | otherwise             = Nothing

getIndexForJumpFrom :: Index -> [Int] -> Maybe Index
getIndexForJumpFrom i ls =
  if isValidListIndex i ls
     then let jump = ls !! i
          in let newIndex = i + jump
             in if isValidListIndex newIndex ls
                   then Just newIndex
                   else Nothing
     else Nothing

isValidListIndex :: Int -> [a] -> Bool
isValidListIndex n ls = n >= 0 && n < length ls


tests = defaultMain $ testGroup "Tests"
  [ incrementListAtIndexTests
  , getIndexForJumpFromTests
  , performJumpTests
  , part1AlgoTests
  ]
  where
    part1AlgoTests = testGroup "part1Algo"
      [ testCase "[0,3,0,1,-3]" $ part1Algo [0,3,0,1,-3] @?= 5
      ]
    incrementListAtIndexTests = testGroup "incrementListAtIndex"
      [ testCase "0 index valid" $ incrementListAtIndex 0 [0..2] @?= Just [1,1,2]
      , testCase "1 index valid" $ incrementListAtIndex 1 [0..2] @?= Just [0,2,2]
      , testCase "2 index valid" $ incrementListAtIndex 2 [0..2] @?= Just [0,1,3]
      , testCase "3 index not valid" $ incrementListAtIndex 3 [0..2] @?= Nothing
      , testCase "-1 index not valid" $ incrementListAtIndex (-1) [0..2] @?= Nothing
      ]
    getIndexForJumpFromTests = testGroup "getIndexForJumpFrom"
      [ testCase "valid forward jump" $ getIndexForJumpFrom 0 ([3,-2,2,-3] :: [Int]) @?= Just 3
      , testCase "invalid reverse jump" $ getIndexForJumpFrom 1 ([3,-2,2,-3] :: [Int]) @?= Nothing
      , testCase "invalid forward jump" $ getIndexForJumpFrom 2 ([3,-2,2,-3] :: [Int]) @?= Nothing
      , testCase "valid reverse jump" $ getIndexForJumpFrom 3 ([3,-2,2,-3] :: [Int]) @?= Just 0
      ]
    performJumpTests = testGroup "getIndexForJumpFrom"
      [ testCase "valid forward jump" $
          performJump (JumpState 0 [3,-2,2,-3]) @?= Just (JumpState 3 [4,-2,2,-3])
      , testCase "invalid reverse jump" $
          performJump (JumpState 1 [3,-2,2,-3]) @?= Nothing
      , testCase "invalid forward jump" $
          performJump (JumpState 2 [3,-2,2,-3]) @?= Nothing
      , testCase "valid reverse jump" $
          performJump (JumpState 3 [3,-2,2,-3]) @?= Just (JumpState 0 [3,-2,2,-2])
      ]
