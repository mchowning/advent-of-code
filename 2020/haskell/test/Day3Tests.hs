{-# LANGUAGE OverloadedStrings #-}

module Day3Tests where

import Data.Text
import Day3
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Util

day3 :: TestTree
day3 =
  testGroup
    "day 3"
    [ testGroup
        "fast"
        [ testCase "parsing" $
            parse' inputParser "" "#.#\n.##"
              @?= [[Tree, Clear, Tree], [Clear, Tree, Tree]],
          testCase "part 1" $
            part1' parsedTestInput
              @?= 7,
          testCase "part 2" $
            part2' parsedTestInput
              @?= 336
        ]
    , testGroup
        "slow"
        [ testCase "part 1" $ part1 >>= (@?= 240)
        , testCase "part 2" $ part2 >>= (@?= 2832009600)
        ]
    ]

parsedTestInput :: [[Square]]
parsedTestInput = parse' inputParser "" testInput

testInput :: Text
testInput =
  "..##.......\n\
  \#...#...#..\n\
  \.#....#..#.\n\
  \..#.#...#.#\n\
  \.#...##..#.\n\
  \..#.##.....\n\
  \.#.#.#....#\n\
  \.#........#\n\
  \#.##...#...\n\
  \#...##....#\n\
  \.#..#...#.#"

-- testInput :: Text
-- testInput =
--   "..##.......\n\
--   \#...#...#..\n\
--   \.#....#..#." -- \n\
--   \..#.#...#.#\n\
--   \.#...##..#.\n\
--   \..#.##.....\n\
--   \.#.#.#....#\n\
--   \.#........#\n\
--   \#.##...#...\n\
--   \#...##....#\n\
--   \.#..#...#.#"
