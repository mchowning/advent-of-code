{-# OPTIONS_GHC -Wall -Werror #-}

module Day5 where

import Data.List
import Test.HUnit
import Text.Printf
--import Control.Monad

results :: IO ()
results = do input <- readFile "day5_input.txt"
             printResult 1 (numPass isNice1Reqs input)
             printResult 2 (numPass isNice2Reqs input)
             -- mapM_ (\(i,f) -> printResult i $ numPass f input) $ zip [1..] [isNice1Reqs, isNice2Reqs]
  where 
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"


numPass :: [String -> Bool] -> String -> Int
numPass fs = length . filter (passesAll fs) . lines

passesAll :: [String -> Bool] -> String -> Bool
passesAll reqs str = all ($ str) reqs

--- part 1

isNice1Reqs :: [String -> Bool]
isNice1Reqs = [ contains3Vowels, hasDoubleLetters, noInvalidPairs ]

contains3Vowels :: String -> Bool
-- contains3Vowels = (>2) . length . filter isVowel
contains3Vowels = containsNVowels 3
  where
    containsNVowels :: Int -> String -> Bool
    containsNVowels n = (>= n) . length . filter isVowel

    isVowel :: Char -> Bool
    isVowel = flip elem "aeiou"

hasDoubleLetters :: String -> Bool
hasDoubleLetters = any ((>1) . length) . group

noInvalidPairs :: String -> Bool
noInvalidPairs str = not . any (`isInfixOf` str) $ ["ab","cd","pq","xy"]

--- part 2

isNice2Reqs :: [String -> Bool]
isNice2Reqs = [ hasTwoPairs, hasSpacedRepeat ]

hasTwoPairs :: String -> Bool
hasTwoPairs (x:y:ys) = isInfixOf [x,y] ys || hasTwoPairs (y:ys)
hasTwoPairs _ = False
-- hasTwoPairs ls = any (uncurry isInfixOf . splitAt 2 . flip drop ls) [0..(length ls - 4)]
-- hasTwoPairs ls = any (uncurry isInfixOf . splitAt 2) . filter ((>3) . length) $ tails ls
-- hasTwoPairs = foldr hasTwoPairs' False . tails
--   where
--     hasTwoPairs' :: String -> Bool -> Bool
--     hasTwoPairs' _ True     = True
--     hasTwoPairs' (x:y:ys) _ = [x,y] `isInfixOf` ys
--     hasTwoPairs' _ _        = False

hasSpacedRepeat :: String -> Bool
hasSpacedRepeat ls = let spacedPairs = zip ls (drop 2 ls)
                     in any (uncurry (==)) spacedPairs


-------------
--- Tests ---
-------------

main :: IO Counts
main = tests

tests :: IO Counts
tests = runTestTT $ TestList [ numPassTests
                             , passesAllTests
                             , noInvalidPairsTests
                             , hasDoubleLettersTests
                             , contains3VowelsTests
                             , hasTwoPairsTests
                             , hasSpacedRepeatTests
                             ]                   

numPassTests :: Test
numPassTests = TestLabel "numPass" $ TestList
  [ 
    0 ~=? numPass [elem 'a']          (unlines [""])
  , 1 ~=? numPass [elem 'a']          (unlines ["a"])
  , 2 ~=? numPass [elem 'a']          (unlines ["a","a"])
  , 2 ~=? numPass [elem 'a']          (unlines ["a","a","b"])
  , 1 ~=? numPass [elem 'a',elem 'b'] (unlines ["a","b","ab"])
  ]

passesAllTests :: Test
passesAllTests = TestLabel "passesAll" $ TestList
  [ passesAll [] "ab"                         ~? "handles empty"
  , passesAll [elem 'a',elem 'b'] "ab"        ~? "passes if satisfies both predicates"
  , not (passesAll [elem 'a',elem 'c'] "ab")  ~? "must pass both predicates"
  ]

contains3VowelsTests :: Test
contains3VowelsTests = TestLabel "contains3VowelsTests" $ TestList
  [ contains3Vowels "ajkaaa"     ~? "True if 4 vowels"
  , contains3Vowels "ajkei"      ~? "True if 3 vowels"
  , not (contains3Vowels "ajki") ~? "False if 3 vowels"
  ]

noInvalidPairsTests :: Test
noInvalidPairsTests = TestLabel "noInvalidPairs" $ TestList 
  [ noInvalidPairs "acpxbdqy"    ~? "valid"
  , noInvalidPairs ""            ~? "handles empty"
  , not (noInvalidPairs "jkab")  ~? "contains \"ab\""
  , not (noInvalidPairs "cdjkb") ~? "contains \"cd\""
  , not (noInvalidPairs "jkapq") ~? "contains \"pq\""
  , not (noInvalidPairs "jxykb") ~? "contains \"xy\""
  ]

hasDoubleLettersTests :: Test
hasDoubleLettersTests = TestLabel "hasDoubleLetters" $ TestList
  [ not (hasDoubleLetters "abcdef") ~? "invalid"
  , not (hasDoubleLetters "")       ~? "empty is invalid"
  , hasDoubleLetters "jklaal"       ~? "contains \"aa\""
  , hasDoubleLetters "jklzzl"       ~? "contains \"zz\""
  , hasDoubleLetters "jklddl"       ~? "contains \"dd\""
  ]

hasTwoPairsTests :: Test
hasTwoPairsTests = TestLabel "hasTwoPairs" $ TestList
  [ hasTwoPairs "xyxy" ~? "minimal valid case" 
  , hasTwoPairs "axyaaxy" ~? "can have letters in between"
  , not (hasTwoPairs "aaa") ~? "pairs may not overlap"
  ]

hasSpacedRepeatTests :: Test
hasSpacedRepeatTests = TestLabel "hasSpacedRepeat" $ TestList
  [ hasSpacedRepeat "xyx" ~? "minimal valid case"
  , hasSpacedRepeat "aaa" ~? "middle can be same"
  , not (hasSpacedRepeat "abca") ~? "not valid if two intervening letters"
  , not (hasSpacedRepeat "aabbcc") ~? "not valid if two in a row"
  ]

