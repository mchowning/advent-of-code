module Day5 (part1, part2) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Vector as V

-- Part 1 is 336905, this takes around 24s not compiled, 12s compiled
-- Part 2 is 21985262, and took 33m compiled

type Index = Int
type ListModifierFunc = Index -> V.Vector Integer -> Maybe (V.Vector Integer)
data JumpState = JumpState Index (V.Vector Integer)
                 deriving (Eq, Show)

part1 :: IO Integer
part1 = part1Algo <$> input

part2 :: IO Integer
part2 = part2Algo <$> input

part1Algo :: V.Vector Integer -> Integer
part1Algo = countJumpsWithMod incrementListAtIndex

part2Algo :: V.Vector Integer -> Integer
part2Algo = countJumpsWithMod part2ModifyListAtIndex

countJumpsWithMod :: ListModifierFunc -> V.Vector Integer -> Integer
countJumpsWithMod f = countJumps f . Just . JumpState 0

countJumps :: ListModifierFunc -> Maybe JumpState -> Integer
countJumps _ Nothing   = 0
countJumps f (Just js) = 1 + countJumps f (performJump f js)

performJump :: ListModifierFunc -> JumpState -> Maybe JumpState
performJump f (JumpState index ls) =
  do newIndex <- getIndexForJumpFrom index ls
     newList <- f index ls
     return $ JumpState newIndex newList

incrementListAtIndex :: Index -> V.Vector Integer -> Maybe (V.Vector Integer)
incrementListAtIndex n ls
  | isValidListIndex n ls = let (before, elementAndAfter) = V.splitAt n ls
                            in Just $ before V.++ V.cons (1 + V.head elementAndAfter)  (V.tail elementAndAfter)
  | otherwise             = Nothing

part2ModifyListAtIndex :: Index -> V.Vector Integer -> Maybe (V.Vector Integer)
part2ModifyListAtIndex n ls
  | isValidListIndex n ls = let (before, elementAndAfter) = V.splitAt n ls
                                newElement = if V.head elementAndAfter >= 3 then V.head elementAndAfter - 1 else V.head elementAndAfter + 1
                            in Just $ before V.++ V.cons newElement (V.tail elementAndAfter)
  | otherwise             = Nothing

getIndexForJumpFrom :: Index -> V.Vector Integer -> Maybe Index
getIndexForJumpFrom i ls =
  if isValidListIndex i ls
     then let jump = ls V.! i
          in let newIndex = i + fromIntegral jump
             in if isValidListIndex newIndex ls
                   then Just newIndex
                   else Nothing
     else Nothing

isValidListIndex :: Int -> V.Vector Integer -> Bool
isValidListIndex n ls = n >= 0 && n < length ls

input :: IO (V.Vector Integer)
input = V.fromList . map read . lines <$> readFile "src/input_day5.txt"

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ incrementListAtIndexTests
  , getIndexForJumpFromTests
  , performJumpTests
  , part1AlgoTest
  , part2AlgoTest
  ]
  where
    part1AlgoTest = testCase "part1 algorithm test" $ part1Algo (V.fromList [0,3,0,1,-3]) @?= 5
    part2AlgoTest = testCase "part2 algorithm test" $ part2Algo (V.fromList [0,3,0,1,-3]) @?= 10
    incrementListAtIndexTests = testGroup "incrementListAtIndex"
      [ testCase "0 index valid" $ incrementListAtIndex 0 (V.fromList [0..2]) @?= Just (V.fromList [1,1,2])
      , testCase "1 index valid" $ incrementListAtIndex 1 (V.fromList [0..2]) @?= Just (V.fromList [0,2,2])
      , testCase "2 index valid" $ incrementListAtIndex 2 (V.fromList [0..2]) @?= Just (V.fromList [0,1,3])
      , testCase "3 index not valid" $ incrementListAtIndex 3 (V.fromList [0..2]) @?= Nothing
      , testCase "-1 index not valid" $ incrementListAtIndex (-1) (V.fromList [0..2]) @?= Nothing
      ]
    getIndexForJumpFromTests = testGroup "getIndexForJumpFrom"
      [ testCase "valid forward jump" $ getIndexForJumpFrom 0 (V.fromList [3,-2,2,-3] :: V.Vector Integer) @?= Just 3
      , testCase "invalid reverse jump" $ getIndexForJumpFrom 1 (V.fromList [3,-2,2,-3] :: V.Vector Integer) @?= Nothing
      , testCase "invalid forward jump" $ getIndexForJumpFrom 2 (V.fromList [3,-2,2,-3] :: V.Vector Integer) @?= Nothing
      , testCase "valid reverse jump" $ getIndexForJumpFrom 3 (V.fromList [3,-2,2,-3] :: V.Vector Integer) @?= Just 0
      ]
    performJumpTests = testGroup "getIndexForJumpFrom"
      [ testCase "valid forward jump" $
          performJump incrementListAtIndex (JumpState 0 (V.fromList [3,-2,2,-3])) @?= Just (JumpState 3 (V.fromList [4,-2,2,-3]))
      , testCase "invalid reverse jump" $
          performJump incrementListAtIndex (JumpState 1 (V.fromList [3,-2,2,-3])) @?= Nothing
      , testCase "invalid forward jump" $
          performJump incrementListAtIndex (JumpState 2 (V.fromList [3,-2,2,-3])) @?= Nothing
      , testCase "valid reverse jump" $
          performJump incrementListAtIndex (JumpState 3 (V.fromList [3,-2,2,-3])) @?= Just (JumpState 0 (V.fromList [3,-2,2,-2]))
      ]
