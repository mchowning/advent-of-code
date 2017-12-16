module Day15 (part1, part2) where

import           Data.Bits        ((.&.))

import           Test.Tasty
import           Test.Tasty.HUnit

part1, part2 :: Int
part1 = part1Algorithm 40000000 input
part2 = part2Algorithm 5000000 input

part1Algorithm, part2Algorithm :: Int -> (Integer, Integer) -> Int
part1Algorithm n (startA, startB) =
  length . filter checkMatch . take n $ zip as bs
 where
  as = drop 1 $ iterate generatorA startA
  bs = drop 1 $ iterate generatorB startB

part2Algorithm n (startA, startB) =
  length . filter checkMatch . take n $ zip as bs
 where
  as = generateLs generatorA 4 startA
  bs = generateLs generatorB 8 startB
  generateLs gen factor start = filter (`divisibleBy` factor) . drop 1 $ iterate gen start
   where
    divisibleBy = (( == 0) .) . mod

generatorA, generatorB :: Integer -> Integer
generatorA = generator 16807
generatorB = generator 48271

generator :: Integer -> Integer -> Integer
generator factor n = n * factor `mod` 2147483647

checkMatch :: (Integer, Integer) -> Bool
checkMatch (a, b) = a .&. 0xffff == b .&. 0xffff

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
    , testCase "generator B + sample input" $ generatorB (snd sampleInput) @?= 430625591 ]
  checkMatchTests = testGroup "check for match"
    [ testCase "1092455, 430625591" $ checkMatch (1092455, 430625591) @?= False
    , testCase "1181022009, 1233683848" $ checkMatch (1181022009, 1233683848) @?= False
    , testCase "245556042, 1431495498" $ checkMatch (245556042, 1431495498) @?= True
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
--    , testCase "sample input" $ part2Algorithm 5000000 sampleInput @?= 309 -- slow
--    , testCase "real input" $ part2Algorithm 5000000 input @?= 320 -- slow
    ]

