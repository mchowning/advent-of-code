{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import Data.Maybe
import Data.List(nub)
import Data.List.Split(splitOn)
import Data.Text(Text)
import qualified Data.Text as T
import Test.HUnit
import Text.Printf

results :: IO ()
results = do input <- readFile "day19_input.txt"
             printResult 1 (result1 input)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

-- numbef of unique strings following a single substitution
-- 509  
result1 :: String -> Int
result1 = length . nub . uncurry allPossibleSubs . parseInput

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
