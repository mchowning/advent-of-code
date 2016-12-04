{-# OPTIONS_GHC -Wall -Werror #-}

module Day16 where

import Control.Arrow((&&&))
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as Map
import Test.HUnit
import Text.Printf(printf)

data Person = Person { name :: String
                     , attrs :: Map.Map String String
                     } deriving (Show, Eq)

results :: IO ()
results = do input <- readFile "day16_input.txt"
             printResult 1 (show . getActualMatch $ input)
             printResult 2 (show . getAdjustedMatch $ input)
  where
    printResult :: Int -> String -> IO ()
    printResult = printf "result %d: %s\n"

valuesToMatch :: Map.Map String String
valuesToMatch = Map.fromList [ ("children"   , "3")
                             , ("cats"       , "7")
                             , ("samoyeds"   , "2")
                             , ("pomeranians", "3")
                             , ("akitas"     , "0")
                             , ("vizslas"    , "0")
                             , ("goldfish"   , "5")
                             , ("trees"      , "5")
                             , ("cars"       , "2")
                             , ("perfumes"   , "1") ]

getActualMatch :: String -> Person
getActualMatch = getMatch doesActualMatch


-- Checks to see if first map contains all keys in second map and that the values match
doesActualMatch :: Map.Map String String -- predicted
                -> Map.Map String String -- actual
                -> Bool
doesActualMatch attributeMap = Map.foldrWithKey (\k v acc -> acc && isAttributeInMap k v) True 
  where
    isAttributeInMap :: String -> String -> Bool
    isAttributeInMap k v = Map.lookup k attributeMap == Just v


getAdjustedMatch :: String -> Person
getAdjustedMatch = getMatch doesAdjustedMatch


getMatch :: (Map.Map String String -> Map.Map String String -> Bool) -> String -> Person
getMatch matcher str = let possibleMatches = map parseLine (lines str)
                       in head $ filter (matcher valuesToMatch . attrs) possibleMatches


-- Checks to see if first map contains all keys in second map and that the values are an 'adjusted' match
doesAdjustedMatch :: Map.Map String String -- predicted
                  -> Map.Map String String -- actual
                  -> Bool
doesAdjustedMatch attributeMap = Map.foldrWithKey (\k v acc -> acc && isAdjustedAttributeInMap k v) True
  where
    isAdjustedAttributeInMap :: String -> String -> Bool
    isAdjustedAttributeInMap k v = let comparator = adjustedComparator k
                                     in Map.lookup k attributeMap `comparator` Just v

    -- Adjustments are that predictions are below actual for 'cats' and 'trees' and above actual for
    -- pomeranians and goldfish
    adjustedComparator :: Ord a 
                       => String 
                       -> a -> a -> Bool
    adjustedComparator k | k `elem` ["cats","trees"]           = (<)
                         | k `elem` ["pomeranians","goldfish"] = (>)
                         | otherwise                           = (==)


parseLine :: String -> Person
parseLine str = let pName = last . words . takeWhile (/= ':') $ str
                    attrStr = unwords . drop 2 . words $ str
                in Person pName (parseAttrs attrStr)
  where
    parseAttrs :: String -> Map.Map String String
    parseAttrs = Map.fromList . map parseAttr . splitOn ", "

    parseAttr :: String -> (String,String)
    parseAttr = takeWhile (/= ':') &&& dropWhile (not . isDigit)
    {-parseAttr str1 = let first = takeWhile (/= ':') str1-}
    {-                     second = dropWhile (not . isDigit) str1-}
    {-                 in (first, second)-}

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseLine testInput1 ~?= Person "1" (Map.fromList [ ("children","1")
                                                      , ("cars"    ,"8")
                                                      , ("vizslas" ,"7")
                                                      ])
                                                      
  , parseLine testInput2 ~?= Person "2" (Map.fromList [ ("akitas"  ,"10")
                                                      , ("perfumes","10")
                                                      , ("children","5")
                                                      ])

  , doesActualMatch (Map.fromList [("children","1")])
                    (Map.fromList [("children","1")])                ~?= True

  , doesActualMatch (Map.fromList [("children","1"),("akitas","1")])
                    (Map.fromList [("children","1")])                ~?= True -- matches if all of second are in first

  , doesActualMatch (Map.fromList [("children","1")])
                    (Map.fromList [("children","1"),("akitas","1")]) ~?= False -- does not match if second has extra attrs

  , doesActualMatch (Map.fromList [("children","1")])
                    (Map.fromList [("children","2")])                ~?= False


  , doesAdjustedMatch (Map.fromList [("children","1")])
                      (Map.fromList [("children","1")])                ~?= True
  , doesAdjustedMatch (Map.fromList [("children","1"),("akitas","1")])
                      (Map.fromList [("children","1")])                ~?= True -- matches if all of second are in first
  , doesAdjustedMatch (Map.fromList [("children","1")])
                      (Map.fromList [("children","1"),("akitas","1")]) ~?= False -- does not match if second has extra attrs
  , doesAdjustedMatch (Map.fromList [("children","1")])
                      (Map.fromList [("children","2")])                ~?= False

  , doesAdjustedMatch (Map.fromList [("cats","1")])
                      (Map.fromList [("cats","1")])        ~?= False
  , doesAdjustedMatch (Map.fromList [("cats","1")])
                      (Map.fromList [("cats","2")])        ~?= True
  , doesAdjustedMatch (Map.fromList [("trees","1")])
                      (Map.fromList [("trees","1")])       ~?= False
  , doesAdjustedMatch (Map.fromList [("trees","1")])
                      (Map.fromList [("trees","2")])       ~?= True
  , doesAdjustedMatch (Map.fromList [("pomeranians","1")])
                      (Map.fromList [("pomeranians","1")]) ~?= False
  , doesAdjustedMatch (Map.fromList [("pomeranians","1")])
                      (Map.fromList [("pomeranians","0")]) ~?= True
  , doesAdjustedMatch (Map.fromList [("goldfish","1")])
                      (Map.fromList [("goldfish","1")])    ~?= False
  , doesAdjustedMatch (Map.fromList [("goldfish","1")])
                      (Map.fromList [("goldfish","0")])    ~?= True
  
  ]

testInput1 :: String
testInput1 = "Sue 1: children: 1, cars: 8, vizslas: 7"
testInput2 :: String
testInput2 = "Sue 2: akitas: 10, perfumes: 10, children: 5"
