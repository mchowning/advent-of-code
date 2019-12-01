{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import Util
import TestHelpers

import Control.Monad (when)
import Data.IntSet (IntSet, insert, member)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (Parsec, ParseErrorBundle, errorBundlePretty, runParser, sepBy1)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.IntSet as IntSet

part1 :: IO Int
part1 = sum <$> readInput

-----------------

part2 :: IO Int
part2 =
  firstDup . scanl1 (+) . cycle <$> readInput

  -- do
  --   input <- readInput
  --   let runningTotal = scanl1 (+) (cycle input)
  --   return (firstDup runningTotal)

firstDup :: [Int] -> Int
firstDup = go IntSet.empty
  where
    go seen (x:xs)
      | x `IntSet.member` seen = x
      | otherwise = go (IntSet.insert x seen) xs

-- firstDup :: [Int] -> Int
-- firstDup ls = evalState (getFirstDuplicate ls) IntSet.empty

-- getFirstDuplicate :: [Int] -> State IntSet Int
-- getFirstDuplicate (n:ns) = do
--   isDuplicate <- gets (member n)
--   if isDuplicate
--     then return n
--     else do
--       modify (insert n)
--       getFirstDuplicate ns

-----------------

readInput :: IO [Int]
readInput = parseInput (signedInt `sepBy1` eol) "day1.txt"
  where
    signedInt = signed (return ()) decimal

test :: IO ()
test = runTestCases (TestCase "part 1" 47  part1)
                    (TestCase "part 2" 790 part2)
