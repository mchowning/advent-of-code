{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Day7 where

import           Day5                       (Index, Instruction (..), Mode (..))
import qualified Day5
import           Util

import           Data.List                  (foldl', permutations)
import           Data.Sequence              (Seq, (<|))
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import           Data.Vector.Unboxed        (Vector, (!), (//))
import qualified Data.Vector.Unboxed        as V

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           Debug.Trace

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' program = maximum (runAllAmps program <$> possibilities)

possibilities :: [[Int]]
possibilities = permutations [0..4]

runAllAmps :: Vector Int -> [Int] -> Int
runAllAmps program = foldl' (\acc i -> head (runProgram2 [i, acc] program)) 0

runProgram1 :: [Int] -> Vector Int -> [Int]
runProgram1 = Day5.runProgram True

--------------------------------------------------------------------


parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = if mod (div op mode) 10 == 0 then Position else Immediate
          powers = 100 : map (*10) powers -- [100,1000,10000,...]

set :: Int -> Int -> [Int] -> [Int]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

intcode :: [Int] -> [Int] -> Int -> [Int]
intcode input program index =
    let (op, mode1:mode2:_) = parseOpcode $ program !! index
        arg :: Mode -> Int -> Int
        arg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Immediate -> immediate
                            Position -> program !! immediate
    in case op of
           -- add
           1 -> intcode input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> intcode (tail input) newprogram $ index + 2
                    where newprogram = set (arg Immediate 1) (head input) program
           -- output
           4 -> arg mode1 1 : intcode input program (index + 2)
           -- jump if true
           5 -> intcode input program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode input program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 == arg mode2 2 then 1 else 0) program
           -- halt
           99 -> []
           -- unknown
           x -> error $ "invalid opcode: " ++ show x

amplify :: [Int] -> [Int] -> Int
amplify program [a,b,c,d,e] = last oute
    where outa = intcode (a:0:oute) program 0
          outb = intcode (b:outa) program 0
          outc = intcode (c:outb) program 0
          outd = intcode (d:outc) program 0
          oute = intcode (e:outd) program 0



--------------------------------------------------------------------

-- Amp E loops back to Amp A
-- Phase settings are now in the range 5..9
-- Again, each phase setting is used exactly once
-- "Provide each amplifier its phase setting at its first input instruction;
--     ...all further input/output instructions are for signals."

data Amp = Amp { identifier :: Char
--               , inputs  :: Seq Int
--               , outputs :: Seq Int
               , inputs  :: [Int]
               , index   :: Int
               , program :: Vector Int
               }
--         | AmpFinished
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

makeAmp :: Vector Int -> (PhaseSetting, Char) -> Amp
makeAmp program (phase, c) = Amp c [phase] 0 program

runAmp :: Amp -> [Int]
runAmp amp@Amp { identifier, inputs, index, program } =
  let mInstruction = parseInstruction (program ! index)
  in case processInstruction program index <$> mInstruction of

    Just (Add v1 v2 v3) ->
      let newVal = v1 + v2
          newProgram = putValue program v3 newVal
          newIndex = if index == v3 then index else index + 4
      in  runAmp ( amp {index = newIndex, program = newProgram })

    Just (Multiply v1 v2 v3) ->
      let newVal = v1 * v2
          newProgram = putValue program v3 newVal
          newIndex = if index == v3 then index else index + 4
      in  runAmp ( amp {index = newIndex, program = newProgram })

    Just (Input v) ->
          let (i:newInputs) = inputs
              newProgram = putValue program v i
              newIndex = if index == v then index else index + 2
          in runAmp (amp {inputs = newInputs, index = newIndex, program = newProgram})

    Just (Output v) ->
      v : runAmp (amp {index = index + 2 })

    Just (JumpIfTrue v1 v2) ->
      runAmp $ case v1 of
        0 -> amp { index = index + 3 }
        _ -> amp { index = v2 }

    Just (JumpIfFalse v1 v2) ->
      runAmp $ case v1 of
        0 -> amp { index = v2 }
        _ -> amp { index = index + 3 }

    Just (LessThan v1 v2 v3) ->
      let newVal = if v1 < v2 then 1 else 0
          newProgram = putValue program v3 newVal
          newIndex = if index == v3 then index else index + 4
      in runAmp (amp { index = newIndex, program = newProgram })

    Just (Equals v1 v2 v3) ->
      let newVal = if v1 == v2 then 1 else 0
          newProgram = putValue program v3 newVal
          newIndex = if index == v3 then index else index + 4
      in runAmp (amp { index = newIndex, program = newProgram })

    Nothing -> []



runProgram2 :: [Int] -> Vector Int -> [Int]
runProgram2 ns v = runProgram2' ns 0 v []

-- TODO: I don't know why ignoring the index rule works
runProgram2' :: [Int] -> Index -> Vector Int -> [Int] -> [Int]
runProgram2' inputs index vs acc =
  case processInstruction vs index <$> parseInstruction (vs ! index) of
    Just (Add v1 v2 v3) ->
      let newVal = v1 + v2
          newVs = putValue vs v3 newVal
          newIndex = if index == v3 then index else index + 4
      in  runProgram2' inputs newIndex newVs acc
    Just (Multiply v1 v2 v3) ->
      let newVal = v1 * v2
          newVs = putValue vs v3 newVal
          newIndex = if index == v3 then index else index + 4
      in runProgram2' inputs newIndex newVs acc
    Just (Input v) ->
      let newVs = putValue vs v (head inputs)
          newIndex = if index == v then index else index + 2
      in runProgram2' (tail inputs) (index + 2) newVs acc
    Just (Output v) ->
      let newAcc = acc ++ [v]
      in runProgram2' inputs (index + 2) vs newAcc
    Just (JumpIfTrue v1 v2) ->
      case v1 of
        0 -> runProgram2' inputs (index + 3) vs acc
        _ -> runProgram2' inputs v2 vs acc
    Just (JumpIfFalse v1 v2) ->
      case v1 of
        0 -> runProgram2' inputs v2 vs acc
        _ -> runProgram2' inputs (index + 3) vs acc
    Just (LessThan v1 v2 v3) ->
      let newVal = if v1 < v2 then 1 else 0
          newVs = putValue vs v3 newVal
          newIndex = if index == v3 then index else index + 4
      in runProgram2' inputs newIndex newVs acc
    Just (Equals v1 v2 v3) ->
      let newVal = if v1 == v2 then 1 else 0
          newVs = putValue vs v3 newVal
          newIndex = if index == v3 then index else index + 4
      in runProgram2' inputs newIndex newVs acc
    Nothing -> acc

processInstruction :: Vector Int -> Index -> Instruction Mode -> Instruction Int
processInstruction v i im =
  case im of
    (Add m1 m2 m3) -> Add (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Multiply m1 m2 m3) -> Multiply (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Input m1)-> Input (getValue m1 v (i+1))
    (Output m) -> Output (getValue m v (i+1))
    (JumpIfTrue m1 m2) -> JumpIfTrue (getValue m1 v (i+1)) (getValue m2 v (i+2))
    (JumpIfFalse m1 m2) -> JumpIfFalse (getValue m1 v (i+1)) (getValue m2 v (i+2))
    (LessThan m1 m2 m3) -> LessThan (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Equals m1 m2 m3) -> Equals (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))

getValue :: Mode -> Vector Int -> Index -> Int
getValue Position v i  = v ! (v ! i)
getValue Immediate v i = v ! i

putValue :: Vector Int -> Index -> Int -> Vector Int
putValue v i n = v // [(i,n)]

parseInstruction :: Int -> Maybe (Instruction Mode)
parseInstruction n =
  let nStr = show n
      (ms,opCode) = splitAt (length nStr - 2) nStr
      instructionParser = case withLength 2 opCode of
        "01" -> Just . parseAdd . withLength 2
        "02" -> Just . parseMultiply . withLength 2
        "03" -> Just . const (Input Immediate)
        "04" -> Just . parseOutput . withLength 1
        "05" -> Just . parseJumpIfTrue . withLength 2
        "06" -> Just . parseJumpIfFalse . withLength 2
        "07" -> Just . parseLessThan . withLength 2
        "08" -> Just . parseEquals . withLength 2
        "99" -> const Nothing
        x    -> error ("invalid op code of " <> x)
  in instructionParser ms

parseAdd :: String -> Instruction Mode
parseAdd s =
  let (m1, m2) = parse2Modes s
  in Add m1 m2 Immediate

parseMultiply :: String -> Instruction Mode
parseMultiply s =
  let (m1, m2) = parse2Modes s
  in Multiply m1 m2 Immediate

parseOutput :: String -> Instruction Mode
parseOutput = Output . parseMode . head

parseJumpIfTrue :: String -> Instruction Mode
parseJumpIfTrue = parse2Params JumpIfTrue

parseJumpIfFalse :: String -> Instruction Mode
parseJumpIfFalse = parse2Params JumpIfFalse

parseLessThan :: String -> Instruction Mode
parseLessThan s =
  let (m1, m2) = parse2Modes s
  in LessThan m1 m2 Immediate

parseEquals :: String -> Instruction Mode
parseEquals s =
  let (m1, m2) = parse2Modes s
  in Equals m1 m2 Immediate

parse3Params :: (Mode -> Mode -> Mode -> Instruction Mode) -> String -> Instruction Mode
parse3Params f = uncurry3 f . parse3Modes

parse2Params :: (Mode -> Mode -> Instruction Mode) -> String -> Instruction Mode
parse2Params f = uncurry f . parse2Modes

parse2Modes :: String -> (Mode, Mode)
parse2Modes s =
  let [m2, m1] = parseMode <$> s
   in (m1, m2)

withLength :: Int -> String -> String
withLength n str =
  if length str == n
    then str
    else withLength n ('0':str)

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

--------------------------------------------------------------------

readInput :: IO (Vector Int)
readInput = Day5.readInputFrom "day7.txt"
