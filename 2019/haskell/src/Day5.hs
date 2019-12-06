{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import           Util

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           Data.Vector.Unboxed        (Vector, (!), (//))
import qualified Data.Vector.Unboxed        as V

import Data.List (intercalate)
import Debug.Trace

type Index = Int

data Mode = Position | Immediate deriving (Eq, Show)

data Instruction = Add Mode Mode Mode
                 | Multiply Mode Mode Mode
                 | Input
                 | Output Mode
                 | JumpIfTrue Mode Mode
                 | JumpIfFalse Mode Mode
                 | LessThan Mode Mode
                 | Equals Mode Mode
                 deriving (Eq, Show)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' is = last (runProgram 1 0 is [])

runProgram :: Int -> Index -> Vector Int -> [Int] -> [Int]
runProgram input index vs acc =
  case parseInstruction (vs ! index) of
    Just (Add m1 m2 m3) ->
      let v1 = getValue m1 vs (index + 1)
          v2 = getValue m2 vs (index + 2)
          newVs = putValue m3 vs (index + 3) (v1 + v2)
      in runProgram input (index + 4) newVs acc
    Just (Multiply m1 m2 m3) ->
      let v1 = getValue m1 vs (index + 1)
          v2 = getValue m2 vs (index + 2)
          newVs = putValue m3 vs (index + 3) (v1 * v2)
      in runProgram input (index + 4) newVs acc
    Just Input ->
      let newVs = putValue Position vs (index + 1) input
      in runProgram input (index + 2) newVs acc
    Just (Output m) ->
      let output = getValue m vs (index + 1)
      in runProgram input (index + 2) vs (acc ++ [output])
    Nothing -> acc

parseInstruction :: Int -> Maybe Instruction
parseInstruction n =
  let nStr = show n
      (ms,opCode) = splitAt (length nStr - 2) nStr
      (instructionParser, numMs) = case withLength 2 opCode of
        "01" -> (Just . parseAdd, 3)
        "02" -> (Just . parseMultiply, 3)
        "03" -> (Just . const Input, 0)
        "04" -> (Just . parseOutput, 1)
        "99" -> (const Nothing, 0)
        x    -> error ("invalid op code of " <> x)
  in instructionParser (withLength numMs ms)

getValue :: Mode -> Vector Int -> Index -> Int
getValue Position v i = v ! (v ! i)
getValue Immediate v i = v ! i

putValue :: Mode -> Vector Int -> Index -> Int -> Vector Int
putValue Position v i n = v // [(v ! i, n)]
putValue Immediate v i n = v // [(i,n)]


withLength :: Int -> String -> String
withLength n str =
  if length str == n
    then str
    else withLength n ('0':str)

parseAdd :: String -> Instruction
parseAdd = uncurry3 Add . parse3Modes

parseMultiply :: String -> Instruction
parseMultiply = uncurry3 Multiply . parse3Modes

parseOutput :: String -> Instruction
parseOutput = Output . parseMode . head

parse3Modes :: String -> (Mode, Mode, Mode)
parse3Modes s =
  let [m3, m2, m1] = parseMode <$> s
   in (m1, m2, m3)

parseMode :: Char -> Mode
parseMode '0' = Position
parseMode '1' = Immediate
parseMode c   = error ("Invalid mode of " <> show c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

--------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: Vector Int -> Int
part2' = runProgram2 5 0

runProgram2 :: Int -> Index -> Vector Int -> Int
runProgram2 input index vs =
  case parseInstruction2 (vs ! index) of
    Just (Add m1 m2 m3) ->
      let v1 = getValue m1 vs (index + 1)
          v2 = getValue m2 vs (index + 2)
          v3 = getValue m3 vs (index + 3)
          newVs = putValue m3 vs (index + 3) (v1 + v2)
          newIndex = if index == v3 then index else index + 4
      in  runProgram2 input newIndex newVs
    Just (Multiply m1 m2 m3) ->
      let v1 = getValue m1 vs (index + 1)
          v2 = getValue m2 vs (index + 2)
          v3 = getValue m3 vs (index + 3)
          newVs = putValue m3 vs (index + 3) (v1 * v2)
          newIndex = if index == v3 then index else index + 4
      in runProgram2 input newIndex newVs
    Just Input ->
      let newVs = putValue Position vs (index + 1) input
      in runProgram2 input (index + 2) newVs
    Just (Output m) -> getValue m vs (index + 1)
    Just (JumpIfTrue m1 m2) ->
      case getValue m1 vs (index + 1) of
        0 -> runProgram2 input (index + 3) vs
        _ -> let newIndex = getValue m2 vs (index + 2)
             in runProgram2 input newIndex vs
    Just (JumpIfFalse m1 m2) ->
      case getValue m1 vs (index + 1) of
        0 -> let newIndex = getValue m2 vs (index + 2)
             in runProgram2 input newIndex vs
        _ -> runProgram2 input (index + 3) vs
    Just (LessThan m1 m2) ->
      let p1 = getValue m1 vs (index + 1)
          p2 = getValue m2 vs (index + 2)
          p3 = getValue Immediate vs (index + 3)
          newVal = if p1 < p2 then 1 else 0
          newVs = putValue Immediate vs p3 newVal
          newIndex = if index == p3 then index else index + 4
      in runProgram2 input newIndex newVs
    Just (Equals m1 m2) -> --trace (show index) $
      let p1 = getValue m1 vs (index + 1)
          p2 = getValue m2 vs (index + 2)
          p3 = getValue Immediate vs (index + 3)
          newVal = if p1 == p2 then 1 else 0
          newVs = putValue Immediate vs p3 newVal
          newIndex = if index == p3 then index else index + 4
      in runProgram2 input newIndex newVs
    Nothing -> error "program terminated with no output"

parseInstruction2 :: Int -> Maybe Instruction
parseInstruction2 n =
  let nStr = show n
      (ms,opCode) = splitAt (length nStr - 2) nStr
      (instructionParser, numMs) = case withLength 2 opCode of
        "01" -> (Just . parseAdd, 3)
        "02" -> (Just . parseMultiply, 3)
        "03" -> (Just . const Input, 0)
        "04" -> (Just . parseOutput, 1)
        "05" -> (Just . parseJumpIfTrue, 2)
        "06" -> (Just . parseJumpIfFalse, 2)
        "07" -> (Just . parseLessThan, 2)
        "08" -> (Just . parseEquals, 2)
        "99" -> (const Nothing, 0)
        x    -> error ("invalid op code of " <> x)
  in instructionParser (withLength numMs ms)

parseJumpIfTrue :: String -> Instruction
parseJumpIfTrue = parse2Params JumpIfTrue

parseJumpIfFalse :: String -> Instruction
parseJumpIfFalse = parse2Params JumpIfFalse

parseLessThan :: String -> Instruction
parseLessThan = parse2Params LessThan

parseEquals :: String -> Instruction
parseEquals = parse2Params Equals

parse3Params :: (Mode -> Mode -> Mode -> Instruction) -> String -> Instruction
parse3Params f = uncurry3 f . parse3Modes

parse2Params :: (Mode -> Mode -> Instruction) -> String -> Instruction
parse2Params f = uncurry f . parse2Modes

parse2Modes :: String -> (Mode, Mode)
parse2Modes s =
  let [m2, m1] = parseMode <$> s
   in (m1, m2)

--------------------------------------------------------------

readInput :: IO (Vector Int)
readInput = V.fromList <$> parseInput (signed (return ()) decimal `sepBy1` char ',') "day5.txt"
