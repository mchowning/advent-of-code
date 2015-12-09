{-# OPTIONS_GHC -Wall -Werror #-}

module Day7 where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word
import Test.HUnit
import Text.Printf
import Text.Read

type Identifier = String

results :: IO ()
results = do input <- readFile "day7_input.txt"
             printResult 1 (getValue "a" input)
             printResult 2 (getValueWithStartValues "a" input [("b",46065)])
  where
    printResult :: Int -> String -> IO ()
    printResult = printf "result %d: %s\n"

getValue :: Identifier -> String -> String
getValue ident = getValueFromMap ident . valueMapper

getValueWithStartValues :: Identifier -> String -> [(Identifier,Word16)] -> String
getValueWithStartValues ident input presets = getValueFromMap ident . valueMapperWithStartValues input $ presets

getValueFromMap :: Identifier -> [(Identifier,Word16)] -> String
getValueFromMap ident = show . snd . head . filter ((== ident) . fst)

valueMapper :: String -> [(Identifier,Word16)]
valueMapper str = mapValues ([],lines str)

valueMapperWithStartValues :: String -> [(Identifier,Word16)] -> [(Identifier,Word16)]
valueMapperWithStartValues str startVals = 
  let filteredInputs = filterInputs startVals $ lines str
      updatedInputs = head . map (`updateInputs` filteredInputs) $ startVals
      --updatedInputs = map (concat . flip updateInputs filteredInputs) startVals
  in mapValues (startVals, updatedInputs)

filterInputs :: [(Identifier,Word16)] -> [String] -> [String]
filterInputs presets = filter (flip notAlreadySet presets . last . splitOn " -> ")
--filterInputs presets = filter (`notAlreadySet` presets) . map (last . splitOn " -> ")

notAlreadySet :: Identifier -> [(Identifier,Word16)] -> Bool
notAlreadySet ident = not . any ((== ident) . fst)

mapValues :: ([(Identifier,Word16)],[String]) -> [(Identifier,Word16)]
mapValues (result,[]) = result
mapValues (result,x:xs) = let mappedValue = mapValue x
                          in if isNothing mappedValue
                             then mapValues (result, xs ++ [x])
                             else let newValue = fromJust mappedValue
                                      newInputs = updateInputs newValue xs
                                  in mapValues (newValue:result, newInputs)

mapValue :: String -> Maybe (Identifier,Word16)
mapValue str1 = let strWords = words str1
                    identifier = last strWords
                    value = parseInput (unwords . init . init $ strWords)
                in if isNothing value then Nothing else Just (identifier, fromJust value)

parseInput :: String -> Maybe Word16
parseInput str2 
  | isJust (readMaybe str2::Maybe Word16) = readMaybe str2
  | "AND" `isInfixOf` str2 = (\[x,y] -> parseAnd (readMaybe x) (readMaybe y)) . splitOn " AND " $ str2
  | "OR" `isInfixOf` str2 = (\[x,y] -> parseOr (readMaybe x) (readMaybe y)) . splitOn " OR " $ str2
  | "LSHIFT" `isInfixOf` str2 = (\[x,y] -> parseShift (readMaybe x) (read y)) . splitOn " LSHIFT " $ str2
  | "RSHIFT" `isInfixOf` str2 = (\[x,y] -> parseShift (readMaybe x) (negate . read $ y)) . splitOn " RSHIFT " $ str2
  | "NOT" `isInfixOf` str2 = parseNot (readMaybe . last . words $ str2)
  | otherwise = Nothing

parseAnd :: Maybe Word16 -> Maybe Word16 -> Maybe Word16
parseAnd (Just x) (Just y) = Just (x .&. y)
parseAnd _ _ = Nothing

parseOr :: Maybe Word16 -> Maybe Word16 -> Maybe Word16
parseOr (Just x) (Just y) = Just (x .|. y)
parseOr _ _ = Nothing

parseShift :: Maybe Word16 -> Int -> Maybe Word16
parseShift Nothing _ = Nothing
parseShift (Just v) n = Just (shift v n)

-- TODO good simple function to use for learning monads
parseNot :: Maybe Word16 -> Maybe Word16
parseNot Nothing = Nothing
parseNot (Just v) = Just (complement v)

updateInputs :: (Identifier,Word16) -> [String] -> [String]
updateInputs (id1,val) = map (replace id1 val)
  where 
    replace :: Identifier -> Word16 -> String -> String
    --replace identifier w = map (\x -> if (x /= identifier) then x else show w) . words 
    replace identifier w line = unwords . map (replaceIfMatch identifier w) . words $ line
    
    replaceIfMatch :: Identifier -> Word16 -> String -> String
    replaceIfMatch ident newVal input = if input /= ident then input else show newVal

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ getValue "d" testInput ~?= "72"
  , getValue "e" testInput ~?= "507"
  , getValue "f" testInput ~?= "492"
  , getValue "g" testInput ~?= "114"
  , getValue "h" testInput ~?= "65412"
  , getValue "i" testInput ~?= "65079"
  , getValue "x" testInput ~?= "123"
  , getValue "yz" testInput ~?= "456"
  , getValue "z" testInput ~?= "456"

  , getValueWithStartValues "b" "a -> b\n10 -> a" [("a",11)] ~?= "11"
  , getValueWithStartValues "x" testInput [("x",1000)] ~?= "1000"
  ]

testInput :: String
testInput = unlines [ "x AND yz -> d"
                    , "456 -> yz"
                    , "x OR yz -> e"
                    , "x LSHIFT 2 -> f"
                    , "yz RSHIFT 2 -> g"
                    , "NOT x -> h"
                    , "yz -> z"
                    , "NOT yz -> i"
                    , "123 -> x"
                    ]

