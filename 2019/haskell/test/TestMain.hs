{-# LANGUAGE OverloadedStrings #-}

--import Day1Tests
--import Day2Tests
--import Day3Tests
--import Day4Tests
--import Day5Tests
--import Day6Tests
--import Day7Tests
import Day8Tests

import           Test.Tasty

main :: IO ()
main = defaultMain $
  testGroup "All tests"
    [ testGroup "fast tests"
      [ --day1Tests
--      , day2Tests
--      , day3Tests
--      , day4Tests
--      , day5Tests
--      , day6Tests
--      , day7Tests
       day8Tests
      ]
    -- , testGroup "slow tests"
    --   [
    --   ]
    ]
