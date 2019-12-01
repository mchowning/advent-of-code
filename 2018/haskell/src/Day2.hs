{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Util
import TestHelpers

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

part1 :: IO Int
part1 = undefined

part2 :: IO Text
part2 = undefined

----

readInput :: IO [Text]
readInput = T.lines <$> T.readFile "../inputs/day2.txt"

test :: IO ()
test = void $ runCase (TestCase "part 1" 6448 part1)
-- test = runTestCases (TestCase "part 1" 6448 part1)
--                     (TestCase "part 2" "evsialkqyiurohzpwucngttmf" part2)