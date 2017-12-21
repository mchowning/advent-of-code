{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day12 where

import           Data.List            (nub)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, many, parse, parseErrorPretty,
                                       sepBy1, some)
import           Text.Megaparsec.Char (digitChar, newline, string)

import           Test.Tasty
import           Test.Tasty.HUnit

part1 :: IO Int
part1 = part1Algorithm <$> input

part1Algorithm :: M.Map Integer [Integer] -> Int
part1Algorithm m = S.size (groupContaining m 0)

groupContaining :: M.Map Integer [Integer] -> Integer -> S.Set Integer
groupContaining = addConnections S.empty

part2 :: IO Int
part2 = part2Algorithm <$> input

part2Algorithm :: M.Map Integer [Integer] -> Int
part2Algorithm = length . groups

groups :: M.Map Integer [Integer] -> [S.Set Integer]
groups m = nub $ map (groupContaining m) (M.keys m)

addConnections
  :: S.Set Integer -> M.Map Integer [Integer] -> Integer -> S.Set Integer
addConnections s m i
  | S.member i s
  = s
  | otherwise
  = let newSet         = S.insert i s
        newConnections = fromJust (M.lookup i m)
        connections    = map (addConnections newSet m) newConnections
    in  S.unions connections

input :: IO (M.Map Integer [Integer])
input =
  let filename = "src/input_day12.txt"
  in  M.fromList
      .   processEither
      .   parse file filename
      <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  file :: Parsec Void String [(Integer, [Integer])]
  file = many line

  line :: Parsec Void String (Integer, [Integer])
  line = do
    k  <- some digitChar
    _  <- string " <-> "
    vs <- some digitChar `sepBy1` string ", "
    _  <- newline
    return (read k, map read vs)


-----------------------------------------------------------------------------------

sampleInput :: M.Map Integer [Integer]
sampleInput = M.fromList
  [ (0, [2])
  , (1, [1])
  , (2, [0, 3, 4])
  , (3, [2, 4])
  , (4, [2, 3, 6])
  , (5, [6])
  , (6, [4, 5])
  ]
tests :: IO ()
tests = defaultMain $ testGroup "Day 12 tests" [part1Tests, part2Tests]
 where
  part1Tests = testGroup
    "part 1"
    [ testCase "sample input" $ part1Algorithm sampleInput @?= 6
    , testCase "real input" $ part1 >>= (@?=145)
    ]
  part2Tests = testGroup
    "part 2"
    [ testCase "sample input" $ part2Algorithm sampleInput @?= 2
    , testCase "real input" $ part2 >>= (@?=207)
    ]
