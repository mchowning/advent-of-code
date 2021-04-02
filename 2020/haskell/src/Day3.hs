{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Util

data Square = Clear | Tree deriving (Eq, Show)
type Hill = [[Square]]
type Slope = (Int, Int)

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

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Hill -> Int
part1' = checkRunForTrees (1,3)

checkRunForTrees :: Slope -> Hill -> Int
checkRunForTrees = checkRun (== Tree)

checkRun :: (Square -> Bool) -> Slope -> Hill -> Int
checkRun pred slope hill = length . filter pred $ run slope hill

run :: Slope -> Hill -> [Square]
run slope hill = case move slope (cycle <$> hill) of
  Nothing -> []
  Just remainingHill ->
    let location = head . head $ remainingHill
     in location : run slope remainingHill

move :: Slope -> Hill -> Maybe Hill
move (dy, dx) hill =
  if length hill > dy
    then Just . (drop dx <$>) . drop dy $ hill
    else Nothing

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