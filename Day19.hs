{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import Data.Maybe
import Data.List(nub)
import Data.List.Split(splitOn)
import Data.Text(Text)
import Test.HUnit
import Text.Printf

import qualified Data.Char as C
import qualified Data.Text as T

results :: IO ()
results = do input <- readFile "day19_input.txt"
             printResult 1 (result1 input)
             printResult 2 (result2 input)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

-- numbef of unique strings following a single substitution
-- 509  
result1 :: String -> Int
result1 = length . nub . uncurry allPossibleSubs . parseInput

{- The rules have formats of these types:
  _
  Rn_Ar
  Rn_Y_Ar
  Rn_Y_Y_Ar
In essence, treat the 'Rn' and 'Ar' as parentheses, the 'Y' and the Y as a comma.
Also, all mapping inputs are a single capitalized letter or a capitalized letter
followed by a lowercase letter. 
 
So you can count the number of elements in the end state by counting the number 
of capitalized letters.  Use this number as the starting point because it 
is the maximum number of transitions you would need to get to the conclusion. 
Subtract 1 from that number because you start with 'e' (which transitions into 2 
elements) for free.  Since 'Rn' and 'Ar' and 'Y' are always additional 'elements' 
you  get from a transition for free, subtract the number of those in the conclusion 
from the previously calculated max possible number of transitions.  Likewise, every 
'Y' element has another as-yet-uncounted element after it (i.e., 'Y' is never at 
the end of a mapping or immediately followed by an 'Rn', 'Ar', or another 'Y'), so 
subtract the number of 'Y' elements a second time.  This gives you the only number of
transitions that can achieve a a transition from 'e' (or any other single starting 
element) to the relevant end state.

Result was 195 for my input. -}
result2 :: String -> Int
result2 str = let molecule = fst . parseInput $ str
                  numSymbols = length . filter C.isUpper . T.unpack $ molecule
                  numParens = numOccurrences "Rn" molecule + numOccurrences "Ar" molecule
                  numComma = numOccurrences "Y" molecule
                  numElementsFollowingComma = numComma
                  numStartingElements = 1
              in numSymbols - numParens - numComma - numElementsFollowingComma - numStartingElements

numOccurrences :: Text -> Text -> Int
numOccurrences matcher = length . filter (T.isPrefixOf matcher) . T.tails


parseInput :: String -> (Text, [(Text,Text)])
parseInput str = let [unparsedMappings,molecule] = splitOn [""] . T.lines . T.pack $ str
                     mappings = map parseMapping unparsedMappings
                 in (head molecule, mappings)
  where
    parseMapping :: Text -> (Text,Text)
    parseMapping t = let [k,_,v] = T.words t in (k,v)

allPossibleSubs :: Text -> [(Text,Text)] -> [Text]
allPossibleSubs t mappings = concatMap (allSubsAt t mappings) [0..T.length t - 1]

allSubsAt :: Text -> [(Text,Text)] -> Int -> [Text]
allSubsAt t mappings n = mapMaybe (subsAt t n) mappings

subsAt :: Text -> Int -> (Text,Text) -> Maybe Text
subsAt t n (k,v) = let (start,end) = T.splitAt n t
                                  in T.append start . T.append v <$> T.stripPrefix k end

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseInput "a => b\naa => bb\n\nccc" ~?= ("ccc", [("a","b"),("aa","bb")])

  , subsAt "abcdef" 0 ("abc",".") ~?= Just ".def"
  , subsAt "abcdef" 0 ("xxx",".") ~?= Nothing
  , subsAt "abcdef" 1 ("bc",".") ~?= Just "a.def"
  , subsAt "abcdef" 1 ("x",".") ~?= Nothing

  , allSubsAt "abcdef" [("b",".")
                                       ,("c","_")
                                       ,("x","X")] 1 ~?= ["a.cdef"]

  , allPossibleSubs "abcdef" [("b",".")
                                     ,("c","_")
                                     ,("x","X")] ~?= ["a.cdef","ab_def"]
  ]
