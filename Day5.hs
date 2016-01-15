{-# OPTIONS_GHC -Wall -Werror #-}

module Day5 where

import Data.List
import Test.HUnit
import Text.Printf

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
isNice1Reqs = [ containsNVowels 3, hasDoubleLetters, noInvalidPairs ]

containsNVowels :: Int -> String -> Bool
containsNVowels n = (>= n) . length . filter isVowel
  where
    isVowel :: Char -> Bool
    isVowel = (`elem` "aeiou")

hasDoubleLetters :: String -> Bool
hasDoubleLetters = any ((>1) . length) . group

noInvalidPairs :: String -> Bool
noInvalidPairs str = not $ any (`isInfixOf` str) ["ab","cd","pq","xy"]
-- noInvalidPairs str = none (`isInfixOf` str) ["ab","cd","pq","xy"]
--   where
--     none :: (a -> Bool) -> [a] -> Bool
--     none f = not . any f

--- part 2

isNice2Reqs :: [String -> Bool]
isNice2Reqs = [ hasTwoPairs, hasSpacedRepeat ]

hasTwoPairs :: String -> Bool
hasTwoPairs (x:y:ys) = [x,y] `isInfixOf` ys || hasTwoPairs (y:ys)
hasTwoPairs _ = False

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
                             , containsNVowelsTests
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

containsNVowelsTests :: Test
containsNVowelsTests = TestLabel "contains3VowelsTests" $ TestList
  [ containsNVowels 3 "ajkaaa"     ~? "True if 4 vowels"
  , containsNVowels 3 "ajkei"      ~? "True if 3 vowels"
  , not (containsNVowels 3 "ajki") ~? "False if 3 vowels"
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

