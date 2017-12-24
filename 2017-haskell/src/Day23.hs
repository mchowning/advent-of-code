{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day23 (part1, part2) where

import qualified Data.Map.Strict            as M
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Register = Char
type Value = Int
type Index = Int
type Program = V.Vector Instruction
type Memory = M.Map Register Value

data ValueSource = Literal Value
                 | Pointer Register
                 deriving Show

data Instruction
  = Set         Register    ValueSource
  | Subtract    Register    ValueSource
  | Multiply    Register    ValueSource
  | JumpNotZero ValueSource ValueSource
  deriving Show
  
-- part 1 is 6241
part1 :: IO Int
part1 = runProgramPart1 <$> input

-- part 2 is 909
part2 :: Int
part2 = length $ filter (not . isPrime) [108100, 108117..125100]

-------------------------------------------------------------------------------------------

runProgramPart1 :: Program -> Int
runProgramPart1 p =
  let (_,n) = runEither (Right (0, p, M.empty), 0) runProgram'
  in n
  
runEither :: (Either a b, Int) -> ((Either a b, Int) -> (Either a b, Int)) -> (Either a b, Int)
runEither i@(Left _, _) _ = i
runEither i@(Right _, _) f = runEither (f i) f

runProgram' :: (Either Memory (Index, Program, Memory), Int) -> (Either Memory (Index, Program, Memory), Int)
runProgram' (Right tup@(index, program, _), n) =
  let newN = if isMul (program V.!? index) then 1+n else n
      newTup = executeInstruction tup
  in (newTup, newN)
 where
  isMul (Just (Multiply _ _)) = True
  isMul _ = False
runProgram' r = r

executeInstruction :: (Index, Program, Memory) -> Either Memory (Index, Program, Memory)
executeInstruction (index, prog, mem) = case getInstruction index of
  Nothing -> Left mem
  Just instr -> case instr of
    Set reg vs                 -> let newMem = M.insert reg (getValue vs) mem
                                  in Right (index+1, prog, newMem)
    Subtract reg vs            -> let newMem = M.insert reg (readRegister reg - getValue vs) mem
                                  in Right (index+1, prog, newMem)
    Multiply reg vs            -> let newMem = M.insert reg (readRegister reg * getValue vs) mem
                                  in Right (index+1, prog, newMem)
    JumpNotZero checkVs destVs -> let offset = if getValue checkVs /= 0 then getValue destVs else 1
                                  in Right (index + offset, prog, mem)
 where
  getValue (Literal v) = v
  getValue (Pointer r) = readRegister r
  
  readRegister r = M.findWithDefault 0 r mem
  getInstruction _ = prog V.!? index

-------------------------------------------------------------------------------------------

isPrime :: Int -> Bool
isPrime n
  | n == 2 = True
  | n == 3 = True
  | n `mod` 2 == 0 = False
  | n `mod` 3 == 0 = False
  | otherwise = go (5, 2)
 where
  go :: (Int, Int) -> Bool
  go (i, w)
    | i * i > n  = True
    | n `mod` i == 0 = False
    | otherwise = go (i+w, 6-w)
    
-------------------------------------------------------------------------------------------

input :: IO Program
input =
  let filename = "src/input_day23.txt"
  in processEither . parse parseFile filename <$> readFile filename
 where
  parseFile = V.fromList <$> parseInstruction `sepBy` eol
  parseInstruction = parseSet <|>
               parseAdd <|>
               parseMultiply <|>
               parseJumpNotZero

  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  parseSet = parseRegisterValue "set" Set
  parseAdd = parseRegisterValue "sub" Subtract
  parseMultiply = parseRegisterValue "mul" Multiply

  parseJumpNotZero = do
    _ <- string "jnz "
    check <- parseValueSource
    _ <- space1
    jump <- parseValueSource
    return (JumpNotZero check jump)

  parseRegisterValue :: String -> (Register -> ValueSource -> Instruction) -> Parsec Void String Instruction
  parseRegisterValue prefix i = do
    _ <- string (prefix ++ " ")
    r <- lowerChar
    _ <- space1
    v <- parseValueSource
    return (i r v)

  parseValueSource = Pointer <$> lowerChar <|> Literal <$> signed mempty decimal
