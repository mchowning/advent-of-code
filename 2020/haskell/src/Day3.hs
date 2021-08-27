{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Functor ((<&>))
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Util
import Control.Applicative (liftA2)

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
part1' = checkRun (== Tree) (1, 3)

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: Hill -> Int
part2' =
  let functions = checkRun (== Tree) <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
   in product . sequence functions

--------------------------------------------------------------------------------------

checkRun :: (Square -> Bool) -> Slope -> Hill -> Int
checkRun pred slope hill = length . filter pred $ run slope hill

run :: Slope -> Hill -> [Square]

-- 665 ms ±  15 ms
run slope hill = case move slope (cycle <$> hill) of
  Nothing -> []
  Just remainingHill ->
    let location = head . head $ remainingHill
     in location : run slope remainingHill
  where
    move :: Slope -> Hill -> Maybe Hill
    move (dy, dx) hill =
      if length hill > dy
        then Just . (drop dx <$>) . drop dy $ hill
        else Nothing

-- run slope hill = case move slope hill of
--   Nothing -> []
--   Just remainingHill ->
--     let location = head . head $ remainingHill
--      in location : run slope remainingHill
--   where
--     move :: Slope -> Hill -> Maybe Hill
--     move (dy, dx) hill =
--       if length hill > dy
--         then Just . take (length hill) . (drop dx <$>) . fmap cycle . drop dy $ hill       -- 678 ms ±  37 ms
--         -- then Just . fmap (\row -> drop dx row <> take dx row) . drop dy $ hill
--         -- then Just . fmap ((drop <> take) dx) . drop dy $ hill                                 --  27 ms ±   1.3 ms
--         else Nothing

-- data Coord = Coord
--   { xCoord :: Int ,
--     yCoord :: Int } deriving (Show)

-- run (dy, dx) hill =
--   -- let infiniteSquareCoords = (\n -> Coord {xCoord = dx * n, yCoord = dy * n}) <$> [1..]
--   -- let infiniteSquareCoords = Coord <$> (dx *) <*> (dy *) <$> [1..]
--   let infiniteSquareCoords = liftA2 Coord (dx *) (dy *) <$> [1..]
--       squareCoordsForRun = takeWhile ((length hill >) . yCoord) infiniteSquareCoords
--    in getSquare hill <$> squareCoordsForRun
--   where
--     getSquare :: [[Square]] -> Coord -> Square
--     getSquare hill (Coord x y) =
--       let row = hill !! y
--        in cycle row !! x                                  -- 1.7 ms ± 140 μs

--       --     wrappedX = x `rem` length row
--       -- in row !! wrappedX                               -- 1.3 ms ± 103 μs
