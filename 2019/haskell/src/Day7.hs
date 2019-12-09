{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import qualified Day5

import           Data.List                  (permutations)
import           Data.Vector.Unboxed        (Vector, (!), (//))

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' program = maximum (ampTest program <$> permutations [0..4])

--------------------------------------------------------------------

-- Amp E loops back to Amp A
-- Phase settings are now in the range 5..9
-- Again, each phase setting is used exactly once
-- "Provide each amplifier its phase setting at its first input instruction;
--     ...all further input/output instructions are for signals."

type Index = Int
type Program = Vector Int

data Mode = Position | Immediate deriving (Eq, Show)

data Op = Add
        | Multiply
        | Input
        | Output
        | JumpIfTrue
        | JumpIfFalse
        | LessThan
        | Equals
        | Completed
        deriving (Eq, Show)

data Amp = Amp { identifier :: Char
               , inputs     :: [Int]
               , index      :: Int
               , program    :: Program
               }
         deriving (Eq, Show)

type PhaseSetting = Int

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: Vector Int -> Int
part2' program = maximum (ampTest program <$> permutations [5..9])

ampTest :: Vector Int -> [PhaseSetting] -> Int
ampTest program [a,b,c,d,e] = last outE
  where
    outA = runAmp Amp { identifier = 'A', inputs = a:0:outE, index = 0, program}
    outB = runAmp Amp { identifier = 'B', inputs = b:outA,   index = 0, program}
    outC = runAmp Amp { identifier = 'C', inputs = c:outB,   index = 0, program}
    outD = runAmp Amp { identifier = 'D', inputs = d:outC,   index = 0, program}
    outE = runAmp Amp { identifier = 'E', inputs = e:outD,   index = 0, program}
ampTest _ ps = error ("unexpected size of PhaseSetting list: " <> show ps)

parseOpCode :: Int -> (Op, (Mode, Mode))
parseOpCode n =
  let op = parseOp (n `mod` 100)
      [mode1, mode2] = parseMode . ((`mod` 10) . div n) <$> [100, 1000]
  in (op, (mode1, mode2))

parseOp :: Int -> Op
parseOp = \case
  1 -> Add
  2 -> Multiply
  3 -> Input
  4 -> Output
  5 -> JumpIfTrue
  6 -> JumpIfFalse
  7 -> LessThan
  8 -> Equals
  99 -> Completed
  x -> error ("unexpected op: " <> show x)

parseMode :: Int -> Mode
parseMode = \case
  0 -> Position
  1 -> Immediate
  n -> error ("Unexpected mode of " <> show n)

runAmp :: Amp -> [Int]
runAmp amp@Amp { inputs, index, program } =
  let (op, (mode1,mode2)) = parseOpCode (param 0 Immediate)
      param3 = param 3 Immediate
  in case op of

       Add ->
         let newVal = param 1 mode1 + param 2 mode2
             newProgram = putValue newVal param3
             newIndex = if index == param3 then index else index + 4
         in  runAmp ( amp {index = newIndex, program = newProgram })

       Multiply ->
         let newVal = param 1 mode1 * param 2 mode2
             newProgram = putValue newVal param3
             newIndex = if index == param3 then index else index + 4
         in  runAmp ( amp {index = newIndex, program = newProgram })

       Input ->
         let (i:newInputs) = inputs
             dest = param 1 Immediate
             newProgram = putValue i dest
             newIndex = if index == dest then index else index + 2
         in runAmp (amp {inputs = newInputs, index = newIndex, program = newProgram})

       Output ->
         -- Enable tail-call recursion
         param 1 mode1 : runAmp (amp {index = index + 2 })

       JumpIfTrue ->
         runAmp $ case param 1 mode1 of
           0 -> amp { index = index + 3 }
           _ -> amp { index = param 2 mode2 }

       JumpIfFalse ->
         runAmp $ case param 1 mode1 of
           0 -> amp { index = param 2 mode2 }
           _ -> amp { index = index + 3 }

       LessThan ->
         let newVal = if param 1 mode1 < param 2 mode2 then 1 else 0
             newProgram = putValue newVal param3
             newIndex = if index == param3 then index else index + 4
         in runAmp (amp { index = newIndex, program = newProgram })

       Equals ->
         let newVal = if param 1 mode1 == param 2 mode2 then 1 else 0
             newProgram = putValue newVal param3
             newIndex = if index == param3 then index else index + 4
         in runAmp (amp { index = newIndex, program = newProgram })

       Completed -> []

  where

    param :: Int -> Mode -> Int
    param offset Position  = program ! (program ! (index + offset))
    param offset Immediate = program !            (index + offset)

    putValue :: Int -> Index -> Program
    putValue n i = program // [(i,n)]

--------------------------------------------------------------------

readInput :: IO (Vector Int)
readInput = Day5.readInputFrom "day7.txt"
