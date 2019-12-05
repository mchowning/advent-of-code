{-# LANGUAGE OverloadedStrings #-}

import Day1Tests
import Day2Tests
import Day3Tests
import Day4Tests

import TestHelpers

import           Text.Megaparsec            (satisfy, sepBy1)
import           Text.Megaparsec.Char       (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)

import Data.Proxy
import           Test.Tasty
import           Test.Tasty.Options
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "All tests"
    [ testGroup "fast tests"
      [ --day1Tests
      -- , day2Tests
      -- , day3Tests
        day4Tests
      ]
    -- , testGroup "slow tests"
    --   [
    --   ]
    ]