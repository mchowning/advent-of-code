{-# LANGUAGE GADTs #-}
module TestHelpers (TestCase (..), runTestCases) where

import Control.Monad (unless)

data TestCase a where
  TestCase :: (Eq a, Show a) => { name :: String,
                                  expected :: a,
                                  actual :: IO a } -> TestCase a

runTestCases :: [TestCase a] -> IO ()
runTestCases tcs = do
  results <- mapM runCase tcs
  putStrLn ""
  unless (and results) (error "Checks failed!")

runCase :: TestCase a -> IO Bool
runCase (TestCase name expected actual) = do
  result <- actual
  let passed = result == expected
  let (color, evaluation) = if passed
        then (green, ":  passed!")
        else (red, ":  FAILED!  Expected " <> show expected <> ", but got " <> show result)
  putStrLn (color <> name <> evaluation <> normal)
  return passed
    where
      red = "\x1b[31m"
      green = "\x1b[32m"
      normal = "\x1b[0m"
