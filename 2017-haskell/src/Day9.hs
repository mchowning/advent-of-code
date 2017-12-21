{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day9 where

import Control.Monad.State
import Test.Tasty (defaultMain,testGroup)
import Test.Tasty.HUnit (testCase,(@?=))

part1 :: IO Int
part1 = scoreGroups <$> input

part2 :: IO Int
part2 = countAllGarbage <$> input

input :: IO String
input = readFile "src/input_day9.txt"

scoreGroups :: String -> Int
scoreGroups = score 1 . removeGarbage . removeIgnoreChars

countAllGarbage :: String -> Int
countAllGarbage = countGarbage . removeIgnoreChars

score :: Int -> String -> Int
score _ [] = 0
score level ls = let (group, remainingGroups) = getGroup ls
                     scoreForGroupContents = score (level+1) (getGroupContents group)
                     scoreForThisGroup = if null group
                                         then 0
                                         else level + scoreForGroupContents
                     scoreForRemainingGroups = score level remainingGroups
                     in scoreForThisGroup + scoreForRemainingGroups
  where
     getGroupContents xs = init (tail xs)

getGroup :: String -> (String, String)
getGroup ('{':xs) = let (subGroup, remaining) = getRestOfGroup xs
                   in ('{':subGroup, remaining)
  where
    getRestOfGroup :: String -> (String, String)
    getRestOfGroup [] = ("", "")
    getRestOfGroup (y:ys)
      | y == '{' = let (subGroup, maybeMoreGroup) = getRestOfGroup ys
                       (restOfGroup, remaining) = getRestOfGroup maybeMoreGroup
                   in ('{' : subGroup ++ restOfGroup, remaining)
      | y == '}' = ("}", ys)
      | otherwise = let (restOfGroup, remaining) = getRestOfGroup ys
                    in (y:restOfGroup, remaining)
getGroup [] = ("", "")
getGroup (_:xs) = getGroup xs


removeIgnoreChars :: String -> String
removeIgnoreChars (x:y:ys)
  | x == '!' = removeIgnoreChars ys
  | otherwise = x : removeIgnoreChars (y:ys)
removeIgnoreChars x = x

removeGarbage :: String -> String
removeGarbage = collectNonGarbage
  where
    collectNonGarbage (x:xs)
      | x == '<'  = dropGarbage xs
      | otherwise = x : collectNonGarbage xs
    collectNonGarbage [] = []
    
    dropGarbage (x:xs)
      | x == '>'  = collectNonGarbage xs
      | otherwise = dropGarbage xs
    dropGarbage [] = []

countGarbage :: String -> Int
countGarbage [] = 0
countGarbage (x:xs)
  | x == '<' = let (n, rest) = countGarbage' xs
               in n + countGarbage rest
  | otherwise = countGarbage xs
  
countGarbage' :: String -> (Int, String)
countGarbage' [] = (0,[])
countGarbage' (y:ys)
  | y == '>'  = (0, ys)
  | otherwise = let (n, rest) = countGarbage' ys
                in (n+1, rest)

-----------------------------------------------------------------------------------------------

-- λ> runState (countGarbage'' "a<<>bc<a>d") (0, False)
-- ("abcd",(2,False))
countGarbage'' :: String -> State (Int, Bool) String
countGarbage'' [] = return []
countGarbage'' (y:ys) = do
  (amountRemoved, wasGarbage) <- get
  let isThisCharGarbage = wasGarbage || y == '<'
      isNextCharGarbage = isThisCharGarbage && y /= '>'
      shouldCount = wasGarbage && y /= '>'
  put (if shouldCount then amountRemoved+1 else amountRemoved, isNextCharGarbage)
  r <- countGarbage'' ys
  return $ if isThisCharGarbage then r else y:r
  
  
-----------------------------------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 9 tests"
  [ removeIgnoreCharsTests
  , removeGarbageTests
  , getGroupTests
  , scoreGroupsTests
  , collectGarbageTests
  ]
  where

    sampleInput1 = "{}"
    sampleInput2 = "{{{}}}"
    sampleInput3 = "{{},{}}"
    sampleInput4 = "{{{},{},{{}}}}"
    sampleInput6 = "{<a>,<a>,<a>,<a>}"
    sampleInput7 = "{{<a>},{<a>},{<a>},{<a>}}"
    sampleInput8 = "{{<!>},{<!>},{<!>},{<a>}}"
    sampleInput9 = "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    sampleInput10 = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    sampleInput11 = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

    collectGarbageTests = testGroup "counting garbage"
      [
        testCase "<>" $ countGarbage (removeIgnoreChars "<>") @?= 0
      , testCase "<random characters>" $ countGarbage (removeIgnoreChars "<random characters>") @?= 17
      , testCase "<<<<>" $ countGarbage (removeIgnoreChars "<<<<>") @?= 3
      , testCase "<{!>}>" $ countGarbage (removeIgnoreChars "<{!>}>") @?= 2
      , testCase "<!!>" $ countGarbage (removeIgnoreChars "<!!>") @?= 0
      , testCase "<!!!>>" $ countGarbage (removeIgnoreChars "<!!!>>") @?= 0
      , testCase "<{o\"i!a,<{i<a>" $ countGarbage (removeIgnoreChars "<{o\"i!a,<{i<a>") @?= 10
      ]
    scoreGroupsTests = testGroup "score groups"
      [
        testCase "sample input 1" $ scoreGroups sampleInput1 @?= 1
      , testCase "sample input 2" $ scoreGroups sampleInput2 @?= 6
      , testCase "sample input 3" $ scoreGroups sampleInput3 @?= 5
      , testCase "sample input 4" $ scoreGroups sampleInput4 @?= 16
      , testCase "sample input 6" $ scoreGroups sampleInput6 @?= 1
      , testCase "sample input 9" $ scoreGroups sampleInput9 @?= 9
      , testCase "sample input 10" $ scoreGroups sampleInput10 @?= 9
      , testCase "sample input 11" $ scoreGroups sampleInput11 @?= 3
      , testCase "sample input 11" $ do
          content <- input
          scoreGroups content @?= 16869
      ]
      
    getGroupTests = testGroup "getting group"
      [ testCase "{}" $ getGroup "{}" @?= ("{}","")
      , testCase "{}{}" $ getGroup "{}{}" @?= ("{}","{}")
      , testCase "{{}}{}" $ getGroup "{{}}{}" @?= ("{{}}","{}")
      , testCase "{}{{}}" $ getGroup "{}{{}}" @?= ("{}","{{}}")
      , testCase "discrdsextra{}{{}}" $ getGroup "{}{{}}" @?= ("{}","{{}}")
      ]
    
    removeIgnoreCharsTests = testGroup "remove !s"
      [ testCase "sample input 8" $ removeIgnoreChars sampleInput8 @?= "{{<},{<},{<},{<a>}}"
      , testCase "sample input 10" $ removeIgnoreChars sampleInput10 @?= "{{<>},{<>},{<>},{<>}}"
      , testCase "sample input 11" $ removeIgnoreChars sampleInput11 @?= "{{<a},{<a},{<a},{<ab>}}"
      ]
      
    removeGarbageTests = testGroup "remove garbage"
      [ testCase "sample input 6" $ removeGarbage sampleInput6 @?= "{,,,}"
      , testCase "sample input 7" $ removeGarbage sampleInput7 @?= "{{},{},{},{}}"
      , testCase "sample input 8" $ removeGarbage sampleInput8 @?= "{{},{},{},{}}"
      , testCase "sample input 9" $ removeGarbage sampleInput9 @?= "{{},{},{},{}}"
      , testCase "sample input 10" $ removeGarbage sampleInput10 @?= "{{},{},{},{}}"
      , testCase "sample input 11" $ removeGarbage sampleInput11 @?= "{{},{},{},{}}"
      ]
