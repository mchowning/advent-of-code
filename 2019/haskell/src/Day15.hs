{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where

import           IntCode

import Control.Applicative (liftA2)
import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Linear.V2

import           Debug.Trace

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

---------------------------------------------------------------------------

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
                           West  -> East
                           East  -> West
                     in filter (exclude /=) (M.keys movementCommands)
             in concatMap (runAllMoves . liftA2 (,) (runMove comp) Just) withoutBacktracking

runMove :: Computer -> Move -> Computer
runMove computer move =
  let moveInteger = fromIntegral (movementCommands M.! move)
      newComputer = computer { inputs = [moveInteger] }
  in runComputer newComputer

movementCommands :: M.Map Move Int
movementCommands = M.fromList [ (North, 1)
                              , (South, 2)
                              , (West, 3)
                              , (East, 4) ]

---------------------------------------------------------------------------

type CellMap = M.Map (V2 Int) Cell

data Cell = Oxygen
          | Wall
          | Empty
          deriving (Eq, Show)

-- 302
part2 :: IO Int
part2 = part2' <$> input

part2' :: Program -> Int
part2' program =
  let cellMap = mapEverything program
      notWalls = M.filter (/= Wall) cellMap
      processed = iterate spreadOxygen notWalls
  in length . takeWhile (any (/= Oxygen) . M.elems) $ processed

spreadOxygen :: CellMap -> CellMap
spreadOxygen cellMap =
  let adjacentToOxygenCells = concatMap (M.keys . adjacentCells) . M.keys . M.filter (== Oxygen) $ cellMap
      updateToOxygen cm c = M.adjust (const Oxygen) c cm
  in foldl' updateToOxygen cellMap adjacentToOxygenCells

statusToCell :: StatusCode -> Cell
statusToCell = \case
  HitWall -> Wall
  MovedToOxygen -> Oxygen
  Moved -> Empty

mapEverything :: Program -> CellMap
mapEverything program = mapEverything' mempty
  where
    mapEverything' :: CellMap -> CellMap
    mapEverything' cellMap = runAllMoves' (V2 0 0) cellMap (initialComputer program)

    runAllMoves' :: V2 Int -> CellMap -> Computer -> CellMap
    runAllMoves' cell visited comp@Computer {..} =
      case state of
        CompletedState -> error "computer completed?"
        RunningState ->
          let status = if null outputs then Moved else readStatusCode (head outputs)
              newCellMap = M.insert cell (statusToCell status) visited
          in case status of
               HitWall -> newCellMap
               _ ->
                 let moves = M.difference (adjacentCells cell) newCellMap
                     process map (v2, m) = runAllMoves' v2 map (runMove comp m)
                 in foldl' process newCellMap (M.toList moves)

adjacentCells :: V2 Int -> M.Map (V2 Int) Move
adjacentCells (V2 x y) = M.fromList [ (V2  x   (y+1), North)
                                    , (V2  x   (y-1), South)
                                    , (V2 (x+1) y   , East)
                                    , (V2 (x-1) y   , West)]
