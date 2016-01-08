{-# OPTIONS_GHC -Wall -Werror #-}

module Day7 where

import Control.Applicative
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

--- part 1

getValue :: Identifier -> String -> String
getValue ident = getValueFromMap ident . valueMapper

getValueFromMap :: Identifier -> [(Identifier,Word16)] -> String
getValueFromMap ident = show . snd . head . filter ((== ident) . fst)

valueMapper :: String -> [(Identifier,Word16)]
valueMapper str = mapValues ([],lines str)

mapValues :: ([(Identifier,Word16)],[String]) -> [(Identifier,Word16)]
mapValues (result,[]) = result
mapValues (result,x:xs) = let mappedValue = mapValue x
                          in if isNothing mappedValue then
                               mapValues (result, xs ++ [x])
                             else
                               let newValue = fromJust mappedValue
                                   newInputs = updateInputs newValue xs
                               in mapValues (newValue:result, newInputs)

-- TODO good place to use Maybe as Monad
mapValue :: String -> Maybe (Identifier,Word16)
mapValue str1 = let strWords = words str1
                    identifier = last strWords
                    value = parseInput (unwords . init . init $ strWords)
                in if isNothing value then Nothing else Just (identifier, fromJust value)

-- (>>=) :: Maybe m => m a -> (a -> m b) -> m b
-- ()    :: Maybe m => m a -> m b -> (a -> b -> m c) -> m c


-- TODO use monad to apply all these "parsers" and keep the result of the one that succeeds (library?)
-- TODO probably a good chance to use a Parser Monad
parseInput :: String -> Maybe Word16
parseInput str2
  | isJust (readMaybe str2::Maybe Word16) = readMaybe str2
--   | "AND" `isInfixOf` str2 = (\[x,y] -> (.&.) <$> readMaybe x <*> readMaybe y) . splitOn " AND " $ str2
  | "AND" `isInfixOf` str2 =  foldr1 (liftA2 (.&.)) . map readMaybe . splitOn " AND " $ str2
--   | "OR" `isInfixOf` str2 = (\[x,y] -> (.|.) <$> readMaybe x <*> readMaybe y) . splitOn " OR " $ str2
  | "OR" `isInfixOf` str2 = foldr1 (liftA2 (.|.)) . map readMaybe . splitOn " OR " $ str2
  | "LSHIFT" `isInfixOf` str2 = lShift . splitOn " LSHIFT " $ str2
  | "RSHIFT" `isInfixOf` str2 = rShift . splitOn " RSHIFT " $ str2
  | "NOT" `isInfixOf` str2 = complement <$> (readMaybe . last $ words str2)
  | otherwise = Nothing
    where
      lShift :: [String] -> Maybe Word16
      lShift [w,i] = shift <$> readMaybe w <*> readMaybe i
      lShift _     = undefined

      rShift :: [String] -> Maybe Word16
      rShift [w,i] = shift <$> readMaybe w <*> (negate <$> readMaybe i)
      rShift _     = undefined

updateInputs :: (Identifier,Word16) -> [String] -> [String]
updateInputs (id1,val) = map (replace id1 val)
  where
    replace :: Identifier -> Word16 -> String -> String
    --replace identifier w = map (\x -> if (x /= identifier) then x else show w) . words
    replace identifier w line = unwords . map (replaceIfMatch identifier w) . words $ line

    replaceIfMatch :: Identifier -> Word16 -> String -> String
    replaceIfMatch ident newVal input = if input /= ident then input else show newVal

--- part 2

getValueWithStartValues :: Identifier -> String -> [(Identifier,Word16)] -> String
getValueWithStartValues ident input presets = getValueFromMap ident . valueMapperWithStartValues input $ presets

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

