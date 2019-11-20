{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import Control.Monad (forM_)
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
part1 = sum <$> parseInput

-----------------

part2 :: IO Int
part2 =
  -- firstDup . scanl1 (+) . cycle <$> parseInput

  do
    input <- parseInput
    let runningTotal = scanl1 (+) (cycle input)
    return (firstDup runningTotal)

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
type Parser = Parsec Void Text

parseInput :: IO [Int]
parseInput = parse (signedInt `sepBy1` eol) "input.txt" <$> T.readFile "../input.txt"
  where
    signedInt = signed (return ()) decimal

parse :: Parser a -> Text -> Text -> a
parse parser filename input = processEither (runParser parser (T.unpack filename) input)
  where
    processEither (Left  e ) = error (errorBundlePretty e)
    processEither (Right rs) = rs

test :: IO ()
test = do
  p1 <- part1
  p2 <- part2
  let errors = catMaybes [ check "part 1" p1 470
                         , check "part 2" p2 790]
  if null errors
    then putStrLn "All checks passed!"
    else do
      forM_ errors putStrLn
      error "Checks failed!"
    where
      check desc x expected =
        if x == expected
          then Nothing
          else Just (desc <> " was not " <> show expected <> ", but was " <> show x)

