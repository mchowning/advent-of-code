module Day15 (part1, part2) where

import Text.Printf (printf)

import           Test.Tasty
import           Test.Tasty.HUnit
import Data.List (foldl',foldl1')

type Generator = Integer -> Integer
type NumMatches = Integer
type Result = (NumMatches, Integer, Integer)

part1 :: NumMatches
part1 = part1Algorithm 40000000 input

part2 :: Integer
part2 = part2Algorithm 5000000 input

part1Algorithm :: Int -> (Integer, Integer) -> NumMatches
part1Algorithm = matchesInNTimes generatorA generatorB

part2Algorithm :: Int -> (Integer, Integer) -> NumMatches
part2Algorithm = matchesInNTimes pickyGeneratorA pickyGeneratorB

matchesInNTimes :: Generator -> Generator -> Int -> (Integer, Integer) -> NumMatches
matchesInNTimes g1 g2 n (startA, startB) =
  let (result, _, _) = foldr (\_ acc -> matchGenerator g1 g2 acc) (0, startA, startB) [0..n]
--  let (result, _, _) = strictIterate matchGenerator (0, startA, startB) !! (n+1)
  in result
-- where
--   strictIterate f x =
--       let x' = f x
--       in x' `seq` (x : strictIterate f x')

matchGenerator :: Generator -> Generator -> Result -> Result
matchGenerator ga gb (matches, a, b) =
  let newA = ga a
      newB = gb b
      newMatches = if checkMatch a b
                     then 1 + matches
                     else matches
  in (newMatches, newA, newB)

generatorA :: Generator
generatorA = generator 16807

generatorB :: Generator
generatorB = generator 48271

pickyGeneratorA :: Generator
pickyGeneratorA = pickyGenerator 4 generatorA

pickyGeneratorB :: Generator
pickyGeneratorB = pickyGenerator 8 generatorB

pickyGenerator :: Integer -> Generator -> Integer -> Integer
--pickyGenerator gen multipleOf = head . filter (\n -> n `rem` multipleOf == 0) . iterate gen
--pickyGenerator multipleOf = (firstMatch .) . iterate
pickyGenerator multipleOf gen = firstMatch . drop 1 . iterate gen
 where
  firstMatch :: [Integer] -> Integer
  firstMatch = head . filter (\n -> n `rem` multipleOf == 0) -- . drop 1
--  firstMatch = head . filter (== 0 . flip rem multipleOf)

generator :: Integer -> Integer -> Integer
generator factor n = (n * factor) `rem` 2147483647


checkMatch :: Integer -> Integer -> Bool
checkMatch a b = comparisonValue a == comparisonValue b
 where
  comparisonValue = drop 16 . makeBinary
  
  makeBinary :: Integer -> String
  makeBinary = printf "%032b"

--------------------------------------------------------------------

input :: (Integer, Integer)
input = (277, 349)

sampleInput :: (Integer, Integer)
sampleInput = (65, 8921)

tests :: IO ()
tests = defaultMain $ testGroup "Day 15"
  [ generatorTests
  , checkMatchTests
  , part1AlgorithmTests
  , part2AlgorithmTests
  ]
 where
  generatorTests = testGroup "generators"
    [ testCase "generator A + sample input" $ generatorA (fst sampleInput) @?= 1092455
    , testCase "generator B + sample input" $ generatorB (snd sampleInput) @?= 430625591
    , testCase "picky generator A + sample input" $ pickyGeneratorA (fst sampleInput) @?= 1352636452
    , testCase "picky generator B + sample input" $ pickyGeneratorB (snd sampleInput) @?= 1233683848
    ]
  checkMatchTests = testGroup "check for match"
    [ testCase "1092455, 430625591" $ checkMatch 1092455 430625591 @?= False
    , testCase "1181022009, 1233683848" $ checkMatch 1181022009 1233683848 @?= False
    , testCase "245556042, 1431495498" $ checkMatch 245556042 1431495498 @?= True
    ]
  part1AlgorithmTests = testGroup "part 1 algorithm"
    [ testCase "sample input 2 iterations" $ part1Algorithm 2 sampleInput @?= 0
    , testCase "sample input 3 iterations" $ part1Algorithm 3 sampleInput @?= 1
    , testCase "sample input 4 iterations" $ part1Algorithm 4 sampleInput @?= 1
--    , testCase "sample input" $ matchesInNTimes 40000000 sampleInput @?= 588 -- slow
--    , testCase "real input" $ matchesInNTimes 40000000 input @?= 592 -- slow
    ]
  part2AlgorithmTests = testGroup "part 2 algorithm"
    [ testCase "sample input 1055 iterations" $ part2Algorithm 1055 sampleInput @?= 0
    , testCase "sample input 1056 iterations" $ part2Algorithm 1056 sampleInput @?= 1
    , testCase "sample input 1057 iterations" $ part2Algorithm 1057 sampleInput @?= 1
--    , testCase "sample input" $ part2Algorithm 4000000 sampleInput @?= 309 -- slow
--    , testCase "real input" $ part2Algorithm 4000000 input @?= 320 -- slow
    ]

