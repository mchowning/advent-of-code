{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day4 (part1, part2) where

import           Data.List
import           Data.List.Unique

import           Test.Tasty
import           Test.Tasty.HUnit

part1 :: IO Int
part1 = part1Algo <$> input

part2 :: IO Int
part2 = part2Algo <$> input

input :: IO String
input = readFile "src/input_day4.txt"

part1Algo :: String -> Int
part1Algo = numValid noDuplicates

part2Algo :: String -> Int
part2Algo = numValid noAnagrams

numValid :: (String -> Bool) -> String -> Int
numValid f = length . filter f . lines

noDuplicates :: String -> Bool
noDuplicates = allUnique . words

noAnagrams :: String -> Bool
noAnagrams = allUnique . map sort . words

------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ part1AlgoTest
  , part2AlgoTest
  , noDuplicatesTest
  , noAnagramsTest
  ]
  where
    part1AlgoTest = testCase "test part 1 algorithm" $
      do content <- input
         part1Algo content @?= 386
    part2AlgoTest = testCase "test part 2 algorithm" $
      do content <- input
         part2Algo content @?= 208
    noDuplicatesTest = testGroup "noDuplicates"
      [ testCase "'aa bb cc dd ee' has no duplicates" $
          noDuplicates "aa bb cc dd ee" @? "duplicate improperly found"
      , testCase "'aa bb cc dd aa' duplicates 'aa'" $
          not (noDuplicates "aa bb cc dd aa") @? "missed duplicate"
      , testCase "'aa bb cc dd aaa' has no duplicates" $
          noDuplicates "aa bb cc dd aaa" @? "duplicate improperly found"
      ]
    noAnagramsTest = testGroup "noAnagrams"
      [ testCase "'abcde fghij' has no anagrams" $
          noAnagrams "abcde fghij" @? "anagram improperly found"
      , testCase "'abcde xyz ecda' has 2 anagrams of 'abcde'" $
          not (noAnagrams "abcde xyz ecdab") @? "missed anagram"
      , testCase "'a ab abc abd abf abj' has no anagrams" $
          noAnagrams "a ab abc abd abf abj" @? "anagram improperly found"
      , testCase "'iiii oiii ooii oooi oooo' has no anagrams" $
          noAnagrams "iiii oiii ooii oooi oooo" @? "anagram improperly found"
      , testCase "'oiii ioii iioi iiio' has only anagrams" $
          not (noAnagrams "oiii ioii iioi iiio") @? "missed anagram"
      ]
