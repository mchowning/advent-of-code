{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day15 where

import IntCode
import qualified Data.Map.Strict as M

import Debug.Trace

data StatusCode = HitWall
                | Moved
                | MovedToOxygen
                deriving (Eq, Show)

readStatusCode :: Integer -> StatusCode
readStatusCode = \case
  0 -> HitWall
  1 -> Moved
  2 -> MovedToOxygen
  n -> error ("Invalid status code: " <> show n)

data Move = North | South | West | East deriving (Eq, Ord, Show)


input :: IO Program
input = readProgramFrom "day15.txt"

-- 296
part1 :: IO Int
part1 = part1' <$> input

part1' :: Program -> Int
part1' = minimum . fmap (length . outputs) . runAllMoves . (,Nothing) . initialComputer

runAllMoves :: (Computer, Maybe Move) -> [Computer]
runAllMoves (comp@Computer {..}, lastMove) =
  case state of
    CompletedState -> error "computer completed?"
    RunningState ->
      let status = if null outputs then Moved else readStatusCode (head outputs)
      in case status of
           MovedToOxygen -> [comp]
           HitWall -> []
           Moved ->
             let withoutBacktracking = case lastMove of
                   Nothing -> M.keys movementCommands
                   Just m ->
                     let exclude = case m of
                           North -> South
                           South -> North
                           West -> East
                           East -> West
                     in filter (exclude /=) (M.keys movementCommands)
             in concatMap (runMove comp) withoutBacktracking


runMove :: Computer -> Move -> [Computer]
runMove computer move =
  let moveInteger = fromIntegral (movementCommands M.! move)
      newComputer = computer { inputs = [moveInteger] }
   in runAllMoves (runComputer newComputer, Just move)

movementCommands :: M.Map Move Int
movementCommands = M.fromList [ (North, 1)
                              , (South, 2)
                              , (West, 3)
                              , (East, 4) ]

