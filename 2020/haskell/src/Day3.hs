{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Util

readInput :: IO Hill
readInput = parseInput inputParser "../inputs/3.txt"

inputParser :: Parser Hill
inputParser = some squareParser `sepBy1` eol

squareParser :: Parser Square
squareParser =
  choice
    [ Clear <$ char '.',
      Tree <$ char '#'
    ]

--------------------------------------------------------------------------------------

data Square = Clear | Tree deriving (Eq, Show)

type Hill = [[Square]]

type Slope = (Int, Int)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Hill -> Int
part1' = checkRunForTrees (1, 3)

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: Hill -> Int
part2' =
  let functions = checkRunForTrees <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
   in product . sequence functions

-- part2' ls =
--     let functions = checkRunForTrees <$> [(1,1), (1,3), (1,5), (1,7), (2,1)]
--         treesOnEachRun = fmap ($ ls) functions
--     in product treesOnEachRun

--------------------------------------------------------------------------------------

checkRunForTrees :: Slope -> Hill -> Int
checkRunForTrees slope = length . filter (== Tree) . run slope
-- checkRunForTrees =
--     let numMatching = length . filter (== Tree)
--     in (numMatching .) . run


-- use tail because the start row doesn't seem to be counted according to the example
run :: Slope -> Hill -> [Square]
-- part 1 bench: 1.2 ms ±  68 μs
-- part 2 bench: 2.2 ms ± 119 μs
run (dy, dx) hill = 
  [ row !! columnIndex
  | (x, y) <- tail (zip [0, dx..] [0, dy.. length hill - 1])
  , let row = hill !! y
  , let columnIndex = x `rem` length row ]

-- part 1 bench: 346 ms ±  32 ms
-- part 2 bench: 1.97 s ± 114 ms
-- run slope@(dy,dx) hill = case move (cycle <$> hill) of
--   Nothing -> []
--   Just remainingHill ->
--     let location = head . head $ remainingHill
--     in location : run slope remainingHill
--   where
--     move :: Hill -> Maybe Hill
--     move hill = do
--       guard (length hill > dy)
--       Just (drop dx <$> drop dy hill)