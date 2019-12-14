{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day13 where

import IntCode

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)

import Debug.Trace

input :: IO Program
input = readProgramFrom "day13.txt"

data Tile = Empty
          | Wall
          | Block
          | Paddle
          | Ball
          deriving (Eq, Show)

data Output = Score Integer
            | Pos (Int, Int) Tile
            deriving (Show)

isScore :: Output -> Bool
isScore (Score _) = True
isScore _ = False

part1 :: IO Int
part1 = part1' <$> input

part1' :: Program -> Int
part1' p =
  let output = parseComputerOutput' (startComputer [] p)
  in length . filter isBlock $ output
    where
      isBlock (Pos _ Block) = True
      isBlock _ = False


-- 15706
part2 :: IO Output
part2 = part2' <$> input

part2' :: Program -> Output
part2' p =
  let
      updatedProgram = 2 : tail p
      outputs = parseComputerOutput' (startComputer moves updatedProgram)
      moves = makeMoves outputs Nothing Nothing
  in last (filter isScore outputs)

makeMoves :: [Output] -> Maybe (Int, Int) -> Maybe (Int, Int) -> [Integer]
makeMoves os (Just (bx,_)) paddle@(Just (px,_)) =
  let move = fromIntegral . signum $ bx - px
  in move : makeMoves os Nothing paddle
makeMoves (Score n: os) mBall mPaddle =
  makeMoves os mBall mPaddle
makeMoves (Pos (x,y) Ball :os) _ mPaddle =
  makeMoves os (Just (x,y)) mPaddle
makeMoves (Pos (x,y) Paddle :os) mBall _ =
  makeMoves os mBall (Just (x,y))
makeMoves (Pos _ _ :os) mBall mPaddle =
  makeMoves os mBall mPaddle
makeMoves [] _ _ = []

readScores :: [Integer] -> [Integer]
readScores (-1 : 0 : score : rest) = score : readScores rest
readScores [] = []
readScores xs = error ("computer score output was not divisible by 3. Ended with: " <> show xs)

parseComputerOutput' :: [Integer] -> [Output]
parseComputerOutput' (-1 : 0 : score : rest) =
  Score score : parseComputerOutput' rest
parseComputerOutput' (x : y : t : rest) =
  Pos (fromIntegral x, fromIntegral y) (toTile t) : parseComputerOutput' rest
parseComputerOutput' [] = []
parseComputerOutput' xs = error ("computer draw instructions were not divisible by 3. Ended with: " <> show xs)

toTile :: Integer -> Tile
toTile = \case
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> Paddle
  4 -> Ball
  n -> error ("unexpected tile integer: " <> show n)
