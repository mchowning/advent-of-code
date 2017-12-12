{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Data.Functor (($>))
import           Data.List            (foldl', maximum)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParseError, Parsec, choice, parse,
                                       parseErrorPretty, sepBy1)
import           Text.Megaparsec.Char (char, string)

import           Test.Tasty
import           Test.Tasty.HUnit

part1 :: IO Integer
part1 = part1Algo <$> input

part2 :: IO Integer
part2 = part2Algo <$> input

data Direction
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Show, Eq)

newtype HexCoord =
  HC (Int, Int, Int)
  deriving (Show, Eq)

move :: HexCoord -> Direction -> HexCoord
move (HC (x, y, z)) = \case
  N  -> HC (x, y + 1, z - 1)
  NE -> HC (x + 1, y, z - 1)
  SE -> HC (x + 1, y - 1, z)
  S  -> HC (x, y - 1, z + 1)
  SW -> HC (x - 1, y, z + 1)
  NW -> HC (x - 1, y + 1, z)

part1Algo :: [Direction] -> Integer
part1Algo = distanceFromRoot . foldl' move (HC (0, 0, 0))

part2Algo :: [Direction] -> Integer
part2Algo = snd . foldl' helper (HC (0, 0, 0), 0)
 where
  helper (hc, maxD) dir =
    let newHc  = move hc dir
        newMax = max maxD (distanceFromRoot newHc)
    in  (newHc, newMax)

input :: IO [Direction]
input = parsedDirections >>= \case
  Left  e  -> error (parseErrorPretty e)
  Right ds -> return ds
 where
  parsedDirections :: IO (Either (ParseError Char Void) [Direction])
  parsedDirections =
    let filename = "src/input_day11.txt"
    in  parse directionParser filename <$> readFile filename


  directionParser :: Parsec Void String [Direction]
  directionParser = choice [nw, ne, n, sw, se, s] `sepBy1` char ','
   where
    nw = string "nw" $> NW
    ne = string "ne" $> NE
    n  = string "n" $> N
    sw = string "sw" $> SW
    se = string "se" $> SE
    s  = string "s" $> S

distanceFromRoot :: HexCoord -> Integer
distanceFromRoot (HC (x, y, z)) = toInteger (maximum [x, y, z])

---------------------------------------------------------------------
tests :: IO ()
tests = defaultMain
  $ testGroup "Day 11 tests" [moveTests, part1AlgoTests, part2Test]
 where
  moveTests = testGroup
    "move"
    [ testCase "N" $ move (HC (0, 0, 0)) N @?= HC (0, 1, -1)
    , testCase "NE" $ move (HC (0, 0, 0)) NE @?= HC (1, 0, -1)
    , testCase "SE" $ move (HC (0, 0, 0)) SE @?= HC (1, -1, 0)
    , testCase "S" $ move (HC (0, 0, 0)) S @?= HC (0, -1, 1)
    , testCase "SW" $ move (HC (0, 0, 0)) SW @?= HC (-1, 0, 1)
    , testCase "NW" $ move (HC (0, 0, 0)) NW @?= HC (-1, 1, 0)
    ]
  part1AlgoTests = testGroup
    "part 1 algorithm"
    [ testCase "ne,ne,ne" $ part1Algo [NE, NE, NE] @?= 3
    , testCase "ne,ne,sw,sw" $ part1Algo [NE, NE, SW, SW] @?= 0
    , testCase "ne,ne,s,s" $ part1Algo [NE, NE, S, S] @?= 2
    , testCase "se,sw,se,sw,sw" $ part1Algo [SE, SW, SE, SW, SW] @?= 3
    , testCase "actual input" $ part1 >>= (@?=715)
    ]
  part2Test = testCase "part 2 test on actual input" $ part2 >>= (@?=1512)
