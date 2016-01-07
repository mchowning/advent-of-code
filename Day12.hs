{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Char(isDigit)
import Data.Function(on)
import Data.List(groupBy)
import Data.Maybe
import Test.HUnit
import Text.Read(readMaybe)
import Text.Printf(printf)

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific as S
import qualified Data.Vector as V

import Data.Aeson

results :: IO ()
results = do input <- B.readFile "day12_input.txt"
             printResult 1 (result1 $ show input)
             printResult 2 (result2 input)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

--- Part 1

-- 119433
result1 :: String -> Int
result1 = sum . mapMaybe readMaybe . groupBy areSameCharType

areSameCharType :: Char -> Char -> Bool
areSameCharType = (==) `on` isNumRelatedChar

isNumRelatedChar :: Char -> Bool
isNumRelatedChar = (||) <$> isDigit <*> (== '-')

--- Part 2

-- 68466
result2 :: B.ByteString -> Int
result2 = valueOfObject . decodeJson

decodeJson :: B.ByteString -> Object
decodeJson = fromMaybe parseError . decode
  where
    parseError = error "failed to parse json"

valueOfValue :: Value -> Int
valueOfValue (Number s) = fromJust $ S.toBoundedInteger s
valueOfValue (Array vector)  = V.sum . V.map valueOfValue $ vector
valueOfValue (Object o) = valueOfObject o
valueOfValue _          = 0

valueOfObject :: Object -> Int
valueOfObject o = if isObjectAllowed o then
                    sum . map valueOfValue . H.elems $ o
                  else
                    0
  where
    isObjectAllowed :: Object -> Bool
    isObjectAllowed = all isValueAllowed . H.elems

    isValueAllowed :: Value -> Bool
    isValueAllowed (String "red") = False
    isValueAllowed _              = True


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ TestLabel "isNumRelatedChar" $ TestList
    [ isNumRelatedChar 'a' ~?= False
    , isNumRelatedChar '1' ~?= True
    , isNumRelatedChar '-' ~?= True
    ]

  , TestLabel "result1" $ TestList
    [ result1 "[1,2,3]" ~?= 6
    , result1 "[[[3]]]" ~?= 3
    , result1 "{\"a\":{\"b\":4},\"c\":-1}" ~?= 3
    , result1 "{\"a\":[-1,1]}" ~?= 0
    , result1 "[-1,{\"a\":1}]" ~?= 0
    , result1 "[]" ~?= 0
    , result1 "{}" ~?= 0
    ]

  , TestLabel "result2" $ TestList
    [ result2 sampleObject ~?= 2
    , result2 sampleArray ~?= 12
    , result2 sampleObjectWithRed ~?= 0
    , result2 sampleObjectWithoutRed ~?= 390
    , result2 sampleNestedObject ~?= 12
    , result2 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" ~?= 0
    , result2 "{\"d\":\"blue\",\"e\":[1,2,3,4],\"f\":5}" ~?= 15
    ]
  ]
    where
      sampleObject :: B.ByteString
      sampleObject = "{\"name\":\"Dave\",\"age\":2}"

      sampleArray :: B.ByteString
      sampleArray = "{\"numbers\":[3,4,5]}"

      sampleObjectWithRed :: B.ByteString
      sampleObjectWithRed = "{\"objectThatMustNotBeNamed\":\"red\",\"arrayWithALotOfNumbers\":[12,324,54]}"


      sampleObjectWithoutRed :: B.ByteString
      sampleObjectWithoutRed = "{\"acceptableObject\":\"blue\",\"arrayWithALotOfNumbers\":[12,324,54]}"

      sampleNestedObject :: B.ByteString
      sampleNestedObject = "{\"name\":\"Dave\",\"anotherObject\":{\"numbers\":[3,4,5]}}"
