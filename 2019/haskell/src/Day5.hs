{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import           Util

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           Data.Text                  (Text)
import           Data.Vector.Unboxed        (Vector, (!), (//))
import qualified Data.Vector.Unboxed        as V

import           Debug.Trace

type Index = Int

data Mode = Position | Immediate deriving (Eq, Show)

data Instruction a = Add a a a
                   | Multiply a a a
                   | Input a
                   | Output a
                   | JumpIfTrue a a
                   | JumpIfFalse a a
                   | LessThan a a a
                   | Equals a a a
                   deriving (Eq, Show)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Vector Int -> Int
part1' = last . runProgram False [1]

getValue :: Mode -> Vector Int -> Index -> Int
getValue Position v i  = v ! (v ! i)
getValue Immediate v i = v ! i

--putValue :: Mode -> Vector Int -> Index -> Int -> Vector Int
--putValue Position v i n  = v // [(v ! i, n)]
--putValue Immediate v i n = v // [(i,n)]

putValue :: Vector Int -> Index -> Int -> Vector Int
putValue v i n = v // [(i,n)]

withLength :: Int -> String -> String
withLength n str =
  if length str == n
    then str
    else withLength n ('0':str)

parseOutput :: String -> Instruction Mode
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
part2' = head . runProgram True [5]

runProgram :: Bool -> [Int] -> Vector Int -> [Int]
runProgram isPart2 ns v = runProgram' isPart2 ns 0 v []

-- TODO: I don't know why ignoring the index rule works
runProgram' :: Bool -> [Int] -> Index -> Vector Int -> [Int] -> [Int]
runProgram' isPart2 inputs index vs acc = --trace (unwords ["\n", show index, show vs]) $
  case processInstruction vs index <$> parseInstruction (vs ! index) of
    Just (Add v1 v2 v3) ->
      let newVal = v1 + v2
          newVs = putValue vs v3 newVal
          newIndex = if isPart2 && index == v3 then index else index + 4
      in  runProgram' isPart2 inputs newIndex newVs acc
    Just (Multiply v1 v2 v3) ->
      let newVal = v1 * v2
          newVs = putValue vs v3 newVal
          newIndex = if isPart2 && index == v3 then index else index + 4
      in runProgram' isPart2 inputs newIndex newVs acc
    Just (Input v) ->
      let newVs = putValue vs v (head inputs)
          newIndex = if isPart2 && index == v then index else index + 4
      in runProgram' isPart2 (tail inputs) (index + 2) newVs acc
    Just (Output v) ->
      let newAcc = acc ++ [v]
      in runProgram' isPart2 inputs (index + 2) vs newAcc
    Just (JumpIfTrue v1 v2) ->
      case v1 of
        0 -> runProgram' isPart2 inputs (index + 3) vs acc
        _ -> runProgram' isPart2 inputs v2 vs acc
    Just (JumpIfFalse v1 v2) ->
      case v1 of
        0 -> runProgram' isPart2 inputs v2 vs acc
        _ -> runProgram' isPart2 inputs (index + 3) vs acc
    Just (LessThan v1 v2 v3) ->
      let newVal = if v1 < v2 then 1 else 0
          newVs = putValue vs v3 newVal
          newIndex = if isPart2 && index == v3 then index else index + 4
      in runProgram' isPart2 inputs newIndex newVs acc
    Just (Equals v1 v2 v3) ->
      let newVal = if v1 == v2 then 1 else 0
          newVs = putValue vs v3 newVal
          newIndex = if isPart2 && index == v3 then index else index + 4
      in runProgram' isPart2 inputs newIndex newVs acc
    Nothing -> acc

processInstruction :: Vector Int -> Index -> Instruction Mode -> Instruction Int
processInstruction v i im =
  case im of
    (Add m1 m2 m3) -> Add (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Multiply m1 m2 m3) -> Multiply (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Input m1) -> Input (getValue m1 v (i+1))
    (Output m) -> Output (getValue m v (i+1))
    (JumpIfTrue m1 m2) -> JumpIfTrue (getValue m1 v (i+1)) (getValue m2 v (i+2))
    (JumpIfFalse m1 m2) -> JumpIfFalse (getValue m1 v (i+1)) (getValue m2 v (i+2))
    (LessThan m1 m2 m3) -> LessThan (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))
    (Equals m1 m2 m3) -> Equals (getValue m1 v (i+1)) (getValue m2 v (i+2)) (getValue m3 v (i+3))

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

--------------------------------------------------------------

readInput :: IO (Vector Int)
readInput = readInputFrom "day5.txt"

readInputFrom :: Text -> IO (Vector Int)
readInputFrom = fmap V.fromList . parseInput (signedInt `sepBy1` char ',')
  where signedInt = signed (return()) decimal
