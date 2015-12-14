{-# OPTIONS_GHC -Wall -Werror #-}

module Day12 where

import Control.Monad(liftM2)
import Data.Char(isDigit)
import Data.List(groupBy)
import Data.Maybe
import Test.HUnit
import Text.Printf(printf)
import Text.Read(readMaybe)

results :: IO ()
results = do input <- readFile "day12_input.txt"
             printResult 1 (result1 input)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

result1 :: String -> Int
result1 = sum . mapMaybe readMaybe . groupBy areSameCharType

result2 :: String -> Int
result2 = undefined

isNum :: String -> Bool
isNum str = isJust (readMaybe str :: Maybe Int)

-- TODO find better way to do this
areSameCharType :: Char -> Char -> Bool
areSameCharType c1 c2 = isNumRelatedChar c1 == isNumRelatedChar c2 

isNumRelatedChar :: Char -> Bool
isNumRelatedChar = liftM2 (||) isDigit (== '-')

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ isNumRelatedChar 'a' ~?= False
  , isNumRelatedChar '1' ~?= True
  , isNumRelatedChar '-' ~?= True

  , isNum "12" ~?= True
  , isNum "-5" ~?= True
  , isNum "0" ~?= True
  , isNum "a" ~?= False

  , result1 "[1,2,3]" ~?= 6
  , result1 "[[[3]]]" ~?= 3
  , result1 "{\"a\":{\"b\":4},\"c\":-1}" ~?= 3
  , result1 "{\"a\":[-1,1]}" ~?= 0
  , result1 "[-1,{\"a\":1}]" ~?= 0
  , result1 "[]" ~?= 0
  , result1 "{}" ~?= 0

  --, result2 "[1,2,3]" ~?= 6
  --, result2 "[1,{\"c\":\"red\",\"b\":2},3]" ~?= 4
  --, result2 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" ~?= 0
  --, result2 "[1,\"red\",5]" ~?= 6
  ]
