-- {-# LANGUAGE OverloadedStrings #-}

import Day1Tests
import Day2Tests

import Test.Tasty
import System.Environment

main :: IO ()
main = do
  setEnv "TASTY_HIDE_SUCCESSES" "true" -- only show failed tests
  defaultMain $ 
    testGroup "All tests"
      [ day1Tests 
      , day2Tests
      ]
