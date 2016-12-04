{-# OPTIONS_GHC -Wall -Werror #-}

module Day7 where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List.Split
import Data.Word
import Test.HUnit
import Text.Printf
import Text.Read

import qualified Data.Map.Strict as M

type Identifier = String

results :: IO ()
results = do input <- readFile "day7_input.txt"
             printResult 1 . show $ getValue "a" input
             printResult 2 . show $ getValueWithStartValues "a" input (M.singleton "b" 46065)
  where
    printResult :: Int -> String -> IO ()
    printResult = printf "result %d: %s\n"

--- part 1

getValue :: Identifier -> String -> Maybe Word16
getValue ident = M.lookup ident . mapValues

mapValues :: String -> M.Map Identifier Word16
mapValues ls = mapValues' (M.empty, lines ls)

-- TODO can't use fold because I'm changing the list as I go, but there's probably a better way
-- This algorithm only works if it is possible to solve every assignment, otherwise
-- it will just infinitely loop
mapValues' :: (M.Map Identifier Word16,[String]) -> M.Map Identifier Word16
mapValues' (result,[]) = result
mapValues' (result,x:xs) = case mapValue x of
                            Nothing -> mapValues' (result, xs ++ [x])
                            Just v  -> let mapWithNewValue = uncurry M.insert v result
                                           updatedInputs = updateValues mapWithNewValue xs
                                       in mapValues' (mapWithNewValue, updatedInputs)


mapValue :: String -> Maybe (Identifier,Word16)
mapValue str = do let identifier = last $ words str
                  v <- parseInput $ dropLastWords 2 str
                  Just (identifier, v)
  where
    dropLastWords :: Int -> String -> String
    dropLastWords n = unwords . reverse . drop n . reverse . words


parseInput :: String -> Maybe Word16
-- parseInput str
--   | isJust (readMaybe str::Maybe Word16) = readMaybe str
--   | "AND" `isInfixOf` str =  foldr1 (liftA2 (.&.)) . map readMaybe . splitOn " AND " $ str
--   | "OR" `isInfixOf` str = foldr1 (liftA2 (.|.)) . map readMaybe . splitOn " OR " $ str
--   | "LSHIFT" `isInfixOf` str = lShift . splitOn " LSHIFT " $ str
--   | "RSHIFT" `isInfixOf` str = rShift . splitOn " RSHIFT " $ str
--   | "NOT" `isInfixOf` str = complement <$> (readMaybe . last $ words str)
--   | otherwise = Nothing

-- parseInput str
--   | isJust (readMaybe str::Maybe Word16) = parseJust str
--   | "AND" `isInfixOf` str =  parseAnd str
--   | "OR" `isInfixOf` str = parseOr str
--   | "LSHIFT" `isInfixOf` str = parseLShift str
--   | "RSHIFT" `isInfixOf` str = parseRShift str
--   | "NOT" `isInfixOf` str = parseNot str
--   | otherwise = Nothing

-- parseInput str = parseJust str `mplus`
--                  parseAnd str `mplus`
--                  parseOr str `mplus`
--                  parseLShift str `mplus`
--                  parseRShift str `mplus`
--                  parseNot str

-- parseInput str = let parserFs = [ parseJust
--                                , parseAnd
--                                , parseOr
--                                , parseLShift
--                                , parseRShift
--                                , parseNot ]
--                  in foldl (\acc f -> acc `mplus` f str) Nothing parserFs

-- parseInput str = msum [ parseJust str
--                       , parseAnd str
--                       , parseOr str
--                       , parseLShift str
--                       , parseRShift str
--                       , parseNot str ]

parseInput str = let parsers = [ parseJust, parseAnd, parseOr, parseLShift, parseRShift, parseNot ]
                     parserResults = map ($ str) parsers
                 in msum parserResults
    where
      parseJust :: String -> Maybe Word16
      parseJust = readMaybe

      parseAnd :: String -> Maybe Word16
      -- parseAnd = (\[x,y] -> (.&.) <$> readMaybe x <*> readMaybe y) . splitOn " AND "
      parseAnd = foldr1 (liftA2 (.&.)) . map readMaybe . splitOn " AND "

      parseOr :: String -> Maybe Word16
      -- parseOr = (\[x,y] -> (.|.) <$> readMaybe x <*> readMaybe y) . splitOn " AND "
      parseOr = foldr1 (liftA2 (.|.)) . map readMaybe . splitOn " OR "

      parseLShift :: String -> Maybe Word16
      parseLShift = lShift . splitOn " LSHIFT "
        where
          lShift :: [String] -> Maybe Word16
          lShift [w,i] = shift <$> readMaybe w <*> readMaybe i
          lShift _     = Nothing

      parseRShift :: String -> Maybe Word16
      parseRShift = rShift . splitOn " RSHIFT "
        where
          rShift :: [String] -> Maybe Word16
          rShift [w,i] = shift <$> readMaybe w <*> (negate <$> readMaybe i)
          rShift _     = Nothing

      parseNot :: String -> Maybe Word16
      parseNot s = complement <$> (readMaybe . last . splitOn "NOT " $ s)

updateValues :: M.Map Identifier Word16 -> [String] -> [String]
updateValues vMap = map (unwords . map updateIfMatch . words)
  where
    updateIfMatch :: String -> String
    updateIfMatch var = case M.lookup var vMap of
                        Nothing -> var
                        Just v  -> show v

--- part 2

getValueWithStartValues :: Identifier -> String -> M.Map Identifier Word16 -> Maybe Word16
getValueWithStartValues ident input = M.lookup ident . valueMapperWithStartValues input

valueMapperWithStartValues :: String -> M.Map Identifier Word16 -> M.Map Identifier Word16
valueMapperWithStartValues str startVals =
  let unsetInputs = filterSetVals startVals str
      inputsUpdatedWithStartValues = updateValues startVals unsetInputs
  in mapValues' (startVals, inputsUpdatedWithStartValues)

filterSetVals :: M.Map Identifier Word16 -> String -> [String]
filterSetVals preSetVals = filter (flip M.notMember preSetVals . assignedVar) . lines
  where
    assignedVar :: String -> String
    assignedVar = last . splitOn " -> "


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ TestLabel "parseInput" $ TestList
    [ parseInput "12345"        ~?= Just 12345
    , parseInput "7 AND 4"      ~?= Just 4
    , parseInput "7 OR 3"       ~?= Just 7
    , parseInput "532 LSHIFT 2" ~?= Just 2128
    , parseInput "532 RSHIFT 2" ~?= Just 133
    , parseInput "NOT 34"       ~?= Just 65501
    ]
  , TestLabel "getValue" $ TestList
    [ getValue "d" testInput       ~?= Just 72
    , getValue "e" testInput       ~?= Just 507
    , getValue "f" testInput       ~?= Just 492
    , getValue "g" testInput       ~?= Just 114
    , getValue "h" testInput       ~?= Just 65412
    , getValue "i" testInput       ~?= Just 65079
    , getValue "x" testInput       ~?= Just 123
    , getValue "yz" testInput      ~?= Just 456
    , getValue "z" testInput       ~?= Just 456
    , getValue "invalid" testInput ~?= Nothing
    ]

  , TestLabel "getValueWithStartValues" $ TestList
    [ getValueWithStartValues "b" "a -> b\n10 -> a" (M.singleton "a" 11) ~?= Just 11
    , getValueWithStartValues "x" testInput (M.singleton "x" 1000)       ~?= Just 1000
    ]
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

