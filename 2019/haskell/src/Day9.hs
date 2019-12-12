{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Day9 where

import           Util

import           Day5                (readInputFrom)

import           Data.List           (permutations)
import           Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

import           Debug.Trace


-- relative mode is like position mode, except it refers to itself + the current relative base
  -- for example, a relative base of 0 would refer to itself
  -- this means they can go backwards
  -- opcode 9 adjusts the relative base by the value of its only parameter
-- Memory can be written beyond the initial program
  -- such memory has an initial value of 0
-- computer needs capabilities for large numbers

type Index = Int
type Program = [Integer]

data Mode = Position
          | Immediate
          | Relative
          deriving (Eq, Show)

data Operation = Add
               | Multiply
               | Input
               | Output
               | JumpIfTrue
               | JumpIfFalse
               | LessThan
               | Equals
               | SetRelativeBase
               | Completed
               deriving (Eq, Show)

data Amp = Amp { inputs       :: [Integer]
               , index        :: Int
               , relativeBase :: Int
               , program      :: Program
               }
         deriving (Eq, Show)

type PhaseSetting = Int

part1 :: IO Integer
part1 = head . ampTest 1 <$> readInput

ampTest :: Integer -> Program -> [Integer]
ampTest input program = runAmp Amp { inputs = [input], index = 0, relativeBase = 0, program = infiniteProgram }
  where
    zeros = 0:zeros -- must be lazy recursion
    infiniteProgram = program ++ zeros

parseOpCode :: Int -> (Operation, M.Map Int Mode)
parseOpCode n =
  let op = parseOp (n `mod` 100)
      [mode1, mode2, mode3] = parseMode . ((`mod` 10) . div n) <$> [100, 1000, 10000]
      modeMap = M.fromList [(1, mode1), (2, mode2), (3, mode3)]
  in (op, modeMap)
    where
      parseOp :: Int -> Operation
      parseOp = \case
        1 -> Add
        2 -> Multiply
        3 -> Input
        4 -> Output
        5 -> JumpIfTrue
        6 -> JumpIfFalse
        7 -> LessThan
        8 -> Equals
        9 -> SetRelativeBase
        99 -> Completed
        x -> error ("unexpected op: " <> show x)

      parseMode :: Int -> Mode
      parseMode = \case
        0 -> Position
        1 -> Immediate
        2 -> Relative
        n -> error ("Unexpected mode of " <> show n)

runAmp :: Amp -> [Integer]
runAmp amp@Amp { inputs, index, relativeBase, program } =
  let (op, modeMap) = parseOpCode (fromIntegral (program !! index))

      paramRead :: Int -> Integer
      paramRead offset = case modeMap M.! offset of
        Position  -> program !!                 fromIntegral (program !! (index + offset))
        Immediate ->                                          program !! (index + offset)
        Relative  -> program !! (relativeBase + fromIntegral (program !! (index + offset)))

      paramWrite :: Int -> Integer -> Program
      paramWrite offset value =
        let dest = case modeMap M.! offset of
                      Position -> fromIntegral (program !! (index + offset))
                      Relative -> fromIntegral (program !! (index + offset)) + relativeBase
                      Immediate -> error "unexpected Immediate mode for insertion"
        -- FIXME hacky
        in take dest program ++ value : drop (dest+1) program

  in case op of

       Add ->
         let newVal = paramRead 1 + paramRead 2
             newProgram = paramWrite 3 newVal
         in  runAmp ( amp {index = index + 4, program = newProgram })

       Multiply ->
         let newVal = paramRead 1 * paramRead 2
             newProgram = paramWrite 3 newVal
         in  runAmp ( amp {index = index + 4, program = newProgram })

       Input ->
         let newProgram = paramWrite 1 (head inputs)
         in runAmp (amp {inputs = tail inputs, index = index + 2, program = newProgram})

       Output ->
         -- Enable tail-call recursion
         paramRead 1 : runAmp (amp {index = index + 2 })

       JumpIfTrue ->
         runAmp $ case paramRead 1 of
           0 -> amp { index = index + 3 }
           _ -> amp { index = fromIntegral (paramRead 2) }

       JumpIfFalse ->
         runAmp $ case paramRead 1 of
           0 -> amp { index = fromIntegral (paramRead 2) }
           _ -> amp { index = index + 3 }

       LessThan ->
         let newVal = if paramRead 1 < paramRead 2 then 1 else 0
             newProgram = paramWrite 3 newVal
         in runAmp (amp { index = index + 4, program = newProgram })

       Equals ->
         let newVal = if paramRead 1 == paramRead 2 then 1 else 0
             newProgram = paramWrite 3 newVal
         in runAmp (amp { index = index + 4, program = newProgram })

       SetRelativeBase ->
         let newRelativeBase = relativeBase + fromIntegral (paramRead 1)
         in runAmp (amp { index = index + 2, relativeBase = newRelativeBase })


       Completed -> []

-----------------------------------------------------------------------------------

part2 :: IO Integer
part2 = head . ampTest 2 <$> readInput

-----------------------------------------------------------------------------------

readInput :: IO [Integer]
readInput = fmap fromIntegral . V.toList <$> readInputFrom "day9.txt"
