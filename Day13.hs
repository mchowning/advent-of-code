{-# OPTIONS_GHC -Wall -Werror #-}

module Day13 where

import Data.List
import Test.HUnit
import Text.Printf(printf)

data Pref = Pref { person :: Person
                 , nextTo :: Person
                 , amount :: Int 
                 } deriving (Show, Eq)

type Person = String

-- result 1 = 733
-- result 2 = 725

results :: IO ()
results = do input <- readFile "day13_input.txt"
             printResult 1 (maximumTotalHappiness input)
             printResult 2 (maximumTotalHappinessWithExtra input)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

maximumTotalHappiness :: String -> Int
maximumTotalHappiness str = let allPrefs = map parseLine . lines $ str
                                allPeople = nub . map person $ allPrefs
                                allArrangements = permutations allPeople
                            in maximum . map (arrangementHappiness allPrefs) $ allArrangements

maximumTotalHappinessWithExtra :: String -> Int
maximumTotalHappinessWithExtra str = let allInputPrefs = map parseLine . lines $ str
                                         allInputPeople = nub . map person $ allInputPrefs
                                         me = "me"
                                         allPrefs = getAllNeutralPrefs me allInputPeople ++ allInputPrefs
                                         allArrangements = permutations (me:allInputPeople)
                                     in maximum . map (arrangementHappiness allPrefs) $ allArrangements

getAllNeutralPrefs :: Person -> [Person] -> [Pref]
getAllNeutralPrefs p = concatMap (getNeutralPrefs p)

getNeutralPrefs :: Person -> Person -> [Pref]
getNeutralPrefs p1 p2 = [Pref p1 p2 0, Pref p2 p1 0]

arrangementHappiness :: [Pref] -> [Person] -> Int
arrangementHappiness prefs ps = let allPairs = if 2 < length ps 
                                               then map (take 2) . tails $ last ps : ps 
                                               else [ps]
                                in sum . map reciprocalHappiness $ allPairs
  where
    reciprocalHappiness :: [Person] -> Int
    reciprocalHappiness pair = sum . map amount . filter isMatch $ prefs
      where
        isMatch :: Pref -> Bool
        isMatch (Pref pPerson pNextTo _) = all (`elem` pair) [pPerson,pNextTo]



parseLine :: String -> Pref
parseLine str = let [p1,_,c,n,_,_,_,_,_,_,p2] = words str
                    p2WithoutPeriod = init p2
                    change = case c of "gain" -> read n
                                       "lose" -> -(read n)
                                       _      -> undefined
                in Pref p1 p2WithoutPeriod change


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseLine (head . lines $ testInput) ~?= Pref "Alice" "Bob" 54

  , arrangementHappiness testPrefs ["Alice","Bob"] ~?= 137
  , arrangementHappiness testPrefs ["Carol","Bob"] ~?= 53
  , arrangementHappiness testPrefs ["Alice","Bob","Carol"] ~?= 49
  , arrangementHappiness testPrefs ["Bob","Alice","Carol"] ~?= 49

  , maximumTotalHappiness testInput ~?= 330
  ]
    where 
      testPrefs = map parseLine . lines $ testInput
      testInput = "Alice would gain 54 happiness units by sitting next to Bob.\n\
                  \Alice would lose 79 happiness units by sitting next to Carol.\n\
                  \Alice would lose 2 happiness units by sitting next to David.\n\
                  \Bob would gain 83 happiness units by sitting next to Alice.\n\
                  \Bob would lose 7 happiness units by sitting next to Carol.\n\
                  \Bob would lose 63 happiness units by sitting next to David.\n\
                  \Carol would lose 62 happiness units by sitting next to Alice.\n\
                  \Carol would gain 60 happiness units by sitting next to Bob.\n\
                  \Carol would gain 55 happiness units by sitting next to David.\n\
                  \David would gain 46 happiness units by sitting next to Alice.\n\
                  \David would lose 7 happiness units by sitting next to Bob.\n\
                  \David would gain 41 happiness units by sitting next to Carol."
