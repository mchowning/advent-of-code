{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Day18 (part1, part2) where

import           Data.Void                  (Void)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec            (Parsec, optional, sepBy, some, try,
                                             (<|>),parseErrorPretty,parse)
import           Text.Megaparsec.Char       (char, digitChar, lowerChar,
                                             newline, space1, string,eol)
import           Text.Megaparsec.Char.Lexer (signed, decimal)
import qualified Data.Map.Strict as M
import Data.List (foldl')
import qualified Data.Vector as V

type Register = Char
type Value = Integer
type Index = Int
type Program = V.Vector Instruction
type Memory = M.Map Register Value
type PlayedSounds = [Integer]

sentKey :: Register
sentKey = '-'

data ValueSource = Literal Value
                 | Pointer Register
                 deriving Show
                 
data Instruction = Send             ValueSource
                 | Set              Register    ValueSource
                 | Add              Register    ValueSource
                 | Multiply         Register    ValueSource
                 | Mod              Register    ValueSource
                 | Recover          ValueSource
                 | JumpGreaterThan0 ValueSource ValueSource
                 deriving Show
                 
-- 3188 is right for part 1

part1, part2 :: IO Integer
part1 = do
  p <- input
  let updates = iterate (update p) ([], M.empty, 0, False)
      (sentSounds,_,_,_) = head . dropWhile (\(_,_,_,isRecovered) -> not isRecovered) $ updates
  return (head sentSounds)
  
part2 = undefined

update :: Program -> (PlayedSounds, Memory, Index, Bool) -> (PlayedSounds, Memory, Index, Bool)
update p (ss, m, i, _) = case p V.! i of
  Send       (getValue -> n) -> (n : ss, m, i+1, False)
  Set      r (getValue -> n) -> (ss, M.insert r n m, i+1, False)
  Add      r (getValue -> n) -> (ss, updateValue r (+) n, i+1, False)
  Multiply r (getValue -> n) -> (ss, updateValue r (*) n, i+1, False)
  Mod      r (getValue -> n) -> (ss, updateValue r mod n, i+1, False)
  Recover    (getValue -> n) -> (ss, m, i+1, n /= 0)
  JumpGreaterThan0
    (getValue -> check)
    (getValue -> n)          -> if check > 0
                                  then (ss, m, i + fromIntegral n, False)
                                  else (ss, m, i+1, False)
  where
    getValue (Literal n) = n
    getValue (Pointer register) = M.findWithDefault 0 register m
    
    updateValue reg op n = M.insert reg (M.findWithDefault 0 reg m `op` n) m

------------------------------------------------------------------

input :: IO Program
input =
  let filename = "src/input_day18.txt"
  in processEither . parse parseFile filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs
  
  parseFile = V.fromList <$> parseInstruction `sepBy` eol
  parseInstruction = parseSend <|>
               parseSet <|>
               parseAdd <|>
               parseMultiply <|>
               parseMod <|>
               parseRecover <|>
               parseJumpGreaterThan
               
  parseSend = Send <$> (string "snd " *> parseValueSource)
  parseSet = parseRegisterValue "set" Set
  parseAdd = parseRegisterValue "add" Add
  parseMultiply = parseRegisterValue "mul" Multiply
  parseMod = parseRegisterValue "mod" Mod
  parseRecover = Recover <$> (string "rcv " *> parseValueSource)

  parseJumpGreaterThan = do
    _ <- string "jgz "
    check <- parseValueSource
    _ <- space1
    jump <- parseValueSource
    return (JumpGreaterThan0 check jump)

  parseRegisterValue :: String -> (Register -> ValueSource -> Instruction) -> Parsec Void String Instruction
  parseRegisterValue prefix i = do
    _ <- string (prefix ++ " ")
    r <- lowerChar
    _ <- space1
    v <- parseValueSource
    return (i r v)

  parseValueSource = Pointer <$> lowerChar <|> Literal <$> signed mempty decimal

------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 18"
  [
  ]
 where

