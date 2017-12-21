{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day14 (part1, part2) where

import           Day10                     (knotHash)

import           Data.List                 (foldl')
import qualified Data.Set                  as S
import           Math.Geometry.Grid        (neighbours)
import           Math.Geometry.Grid.Square (rectSquareGrid)
import           Test.Tasty
import           Test.Tasty.HUnit

type Coord = (Int, Int)
type Group = S.Set Coord

part1 :: Int
part1 = part1Algorithm input

part2 :: Int
part2 = part2Algorithm input

input :: String
input = "wenycdww"

part1Algorithm :: String -> Int
part1Algorithm = length . filter id . concat . binaryBoolRepresentation

part2Algorithm :: String -> Int
part2Algorithm s =
  let allGroups = foldl' process S.empty . positiveCoords . binaryBoolRepresentation $ s
  in S.size allGroups

positiveCoords :: [[Bool]] -> S.Set Coord
positiveCoords lbs =
  let index1 = length lbs - 1
      index2 = length (head lbs) - 1
      allCoords = (,) <$> [0 .. index1] <*> [0 .. index2]
      bsValueForCoord (i1, i2) = (lbs !! i1) !! i2
  in S.filter bsValueForCoord (S.fromList allCoords)

binaryBoolRepresentation :: String -> [[Bool]]
binaryBoolRepresentation = map (map binToBool . rowBinaryRepresentation) . stringsForAllRows

process :: S.Set Group -> Coord -> S.Set Group
process gs c =
  let ns = getNeighborCoords c
      nGroups = S.filter (hasIntersection ns) gs
  in if null nGroups
     then S.insert (S.singleton c) gs
     else
       let combined = S.insert c (S.unions (S.toList nGroups))
       in S.insert combined gs S.\\ nGroups
 where
  hasIntersection = (((/= 0) . S.size) .) . S.intersection
  getNeighborCoords = S.fromList . neighbours (rectSquareGrid 128 128)

stringsForAllRows :: String -> [String]
stringsForAllRows s = map stringForRow ([0 .. 127] :: [Int])
  where stringForRow n = s ++ '-' : show n

rowBinaryRepresentation :: String -> String
rowBinaryRepresentation = concatMap hexToBinary . knotHash

hexToBinary :: Char -> String
hexToBinary = \case
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'a' -> "1010"
  'b' -> "1011"
  'c' -> "1100"
  'd' -> "1101"
  'e' -> "1110"
  'f' -> "1111"
  x   -> error ("invalid hex digit: " ++ show x)

binToBool :: Char -> Bool
binToBool '0' = False
binToBool '1' = True
binToBool x   = error ("invalid bin digit: " ++ show x)


-------------------------------------------------------------------------------

sampleInput :: String
sampleInput = "flqrgnkx"

tests :: IO ()
tests = defaultMain $ testGroup "Day 14"
  [ part1AlgorithmTests
  , processTests
  , part2AlgorithmTests
  ]
 where
  part1AlgorithmTests = testGroup "part 1 algorithm"
    [ testCase ("sample input: " ++ sampleInput) $ part1Algorithm "flqrgnkx" @?= 8108
    , testCase ("real input: " ++ input) $ part1Algorithm input @?= 8226
    ]
  processTests = testGroup "process"
    [ testCase "1 neighbor in group" $ process (S.singleton (S.singleton (1,1))) (1,2) @?= S.singleton (S.fromList [(1,1), (1,2)])
    , testCase "no neighbors in group" $ process (S.singleton (S.singleton (4,1))) (1,2) @?= S.fromList [S.singleton (4,1), S.singleton (1,2)]
    , testCase "2 neighbors in group" $ process (S.singleton (S.fromList [(1,2),(2,1)])) (1,1) @?= S.singleton (S.fromList [(1,1), (1,2), (2,1)])
    , testCase "2 neighbors in different groups" $ process (S.fromList [S.singleton (1,2), S.singleton (2,1)]) (1,1) @?= S.singleton (S.fromList [(1,1), (1,2), (2,1)])
    ]
  part2AlgorithmTests = testGroup "part 2 algorithm"
    [ testCase ("sample input: " ++ sampleInput) $ part2Algorithm "flqrgnkx" @?= 1242
    , testCase ("real input: " ++ input) $ part2Algorithm "flqrgnkx" @?= 1242
    ]
