{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import           Util

import           Prelude                    hiding (Left, Right)

import           Control.Monad              (join, void)

import           Data.Bifunctor             (bimap)
import           Data.List                  (foldl', foldl1', minimumBy, sortBy)
import qualified Data.Map.Strict            as M
import           Data.Ord                   (comparing)
import qualified Data.Set                   as S

import           Text.Megaparsec            (satisfy, sepBy1)
import           Text.Megaparsec.Char       (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)

part1 :: IO Int
part1 = part1' <$> readInput

-- distance to nearest coordinate where the two routes crossed
part1' :: ([Move], [Move]) -> Int
part1' (moves1, moves2) =
  let coordsWhereCrossed = S.intersection (visitedCoords moves1) (visitedCoords moves2)
   in S.findMin (S.map manhattanDistance coordsWhereCrossed)
  where
    visitedCoords = S.fromList . route
    manhattanDistance (a, b) = abs a + abs b

route :: [Move] -> [Coord]
route = route' (0, 0)
  where
    route' :: Coord -> [Move] -> [Coord]
    route' _ [] = []
    route' start (m:ms) =
      let end = findEnd start m
          newPositions = tail (coordsBetweenPoints start end)
       in newPositions <> route' end ms

travel :: Coord -> Move -> [Coord]
travel start move =
  let end = findEnd start move
   in tail (coordsBetweenPoints start end)

coordsBetweenPoints :: Coord -> Coord -> [Coord]
coordsBetweenPoints (x1, y1) (x2, y2) = (,) <$> safeRange x1 x2 <*> safeRange y1 y2

safeRange :: Int -> Int -> [Int]
safeRange a b
  | a < b = [a .. b]
  | a > b = [a,a - 1 .. b]
  | otherwise = [a]

findEnd :: Coord -> Move -> Coord
findEnd (x, y) (Move direction distance) =
  case direction of
    Up    -> (x, y + distance)
    Right -> (x + distance, y)
    Down  -> (x, y - distance)
    Left  -> (x - distance, y)

--------------------------------------------------------------
part2 :: IO Int
part2 = part2' <$> readInput

part2' :: ([Move], [Move]) -> Int
part2' ms =
  let (coordDistance1, coordDistance2) =
        join bimap (shortestDistanceToEachCoord . zip [1 ..] . route) ms
      coordsWhereCrossed = M.intersectionWith (+) coordDistance1 coordDistance2
   in minimum (M.elems coordsWhereCrossed)

shortestDistanceToEachCoord :: [(Int, (Int, Int))] -> M.Map (Int, Int) Int
shortestDistanceToEachCoord = foldl' (\acc (v, k) -> M.insertWith min k v acc) M.empty

--------------------------------------------------------------
data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Eq)

data Move =
  Move Direction Int
  deriving (Show, Eq)

type Coord = (Int, Int)

readInput :: IO ([Move], [Move])
readInput = parseInput parser "day3.txt"

parser :: Parser ([Move], [Move])
parser = do
  moves1 <- moves
  void eol
  moves2 <- moves
  return (moves1, moves2)
  where
    moves = move `sepBy1` char ','

move :: Parser Move
move = do
  rawDirection <- satisfy (`elem` ['U', 'R', 'D', 'L'])
  distance <- decimal
  let direction =
        case rawDirection of
          'U' -> Up
          'R' -> Right
          'D' -> Down
          'L' -> Left
  return (Move direction distance)