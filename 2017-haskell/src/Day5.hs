module Day5 (part1, part2) where

import           Test.Tasty
import           Test.Tasty.HUnit

-- Part 1 is 336905, this takes around 24s not compiled, 12s compiled
-- Part 2 is 21985262, and took 33m compiled

type Index = Int
type ListModifierFunc = Index -> [Int] -> Maybe [Int]
data JumpState = JumpState Index [Int]
                 deriving (Eq, Show)

part1 :: IO Integer
part1 = part1Algo <$> input

part2 :: IO Integer
part2 = part2Algo <$> input

part1Algo :: [Int] -> Integer
part1Algo = countJumpsWithMod incrementListAtIndex

part2Algo :: [Int] -> Integer
part2Algo = countJumpsWithMod part2ModifyListAtIndex

countJumpsWithMod :: ListModifierFunc -> [Int] -> Integer
countJumpsWithMod f = countJumps f . Just . JumpState 0

countJumps :: ListModifierFunc -> Maybe JumpState -> Integer
countJumps _ Nothing   = 0
countJumps f (Just js) = 1 + countJumps f (performJump f js)

performJump :: ListModifierFunc -> JumpState -> Maybe JumpState
performJump f (JumpState index ls) =
  do newIndex <- getIndexForJumpFrom index ls
     newList <- f index ls
     return $ JumpState newIndex newList

incrementListAtIndex :: Index -> [Int] -> Maybe [Int]
incrementListAtIndex n ls
  | isValidListIndex n ls = let (before, element:after) = splitAt n ls
                            in Just $ before ++ (1 + element) : after
  | otherwise             = Nothing

part2ModifyListAtIndex :: Index -> [Int] -> Maybe [Int]
part2ModifyListAtIndex n ls
  | isValidListIndex n ls = let (before, element:after) = splitAt n ls
                                newElement = if element >= 3 then element - 1 else element + 1
                            in Just $ before ++ newElement : after
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

input :: IO [Int]
input = (map read . lines) <$> readFile "src/input_day5.txt"

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ incrementListAtIndexTests
  , getIndexForJumpFromTests
  , performJumpTests
  , part1AlgoTest
  , part2AlgoTest
  ]
  where
    part1AlgoTest = testCase "part1 algorithm test" $ part1Algo [0,3,0,1,-3] @?= 5
    part2AlgoTest = testCase "part2 algorithm test" $ part2Algo [0,3,0,1,-3] @?= 10
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
          performJump incrementListAtIndex (JumpState 0 [3,-2,2,-3]) @?= Just (JumpState 3 [4,-2,2,-3])
      , testCase "invalid reverse jump" $
          performJump incrementListAtIndex (JumpState 1 [3,-2,2,-3]) @?= Nothing
      , testCase "invalid forward jump" $
          performJump incrementListAtIndex (JumpState 2 [3,-2,2,-3]) @?= Nothing
      , testCase "valid reverse jump" $
          performJump incrementListAtIndex (JumpState 3 [3,-2,2,-3]) @?= Just (JumpState 0 [3,-2,2,-2])
      ]
