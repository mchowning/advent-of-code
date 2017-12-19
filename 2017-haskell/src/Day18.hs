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
import qualified Data.Sequence as S

type Register = Char
type Value = Int
type Index = Int
type IsLocked = Bool
type Program = V.Vector Instruction
type Memory = M.Map Register Value
type PlayedSounds = [Int]

sentKey :: Register
sentKey = '-'

data FakeQueue = FakeQueue (S.Seq Value) Int Int
                 deriving Show

data ValueSource = Literal Value
                 | Pointer Register
                 deriving Show
                 
data Instruction = Send             ValueSource
                 | Set              Register    ValueSource
                 | Add              Register    ValueSource
                 | Multiply         Register    ValueSource
                 | Mod              Register    ValueSource
                 | Recover          ValueSource --part1
                 | Receive          Register    --part2
                 | JumpGreaterThan0 ValueSource ValueSource
                 deriving Show
                 
-- 3188 is right for part 1
-- 7112 too high for 1


part1, part2 :: IO Int
part1 = do
  p <- inputForPart1
  let updates = iterate (part1Update p) ([], M.empty, 0, False)
      (sentSounds,_,_,_) = head . dropWhile (\(_,_,_,isRecovered) -> not isRecovered) $ updates
  return (head sentSounds)
  
part2 = do
  p <- inputForPart2
  let (FakeQueue _ n _) = getQueueForProgram1 (part2Algorithm p)
  return n
 where
  getQueueForProgram1 :: (FakeQueue, FakeQueue) -> FakeQueue
  getQueueForProgram1 (q1@(FakeQueue _ _ n1), q2) = if n1 == 1 then q1 else q2

part1Update :: Program -> (PlayedSounds, Memory, Index, Bool) -> (PlayedSounds, Memory, Index, Bool)
part1Update p (ss, m, i, _) = case p V.! i of
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
    
enqueue :: Value -> FakeQueue -> FakeQueue
enqueue v (FakeQueue s amount id) = FakeQueue (v S.<| s) (1+amount) id

pop :: FakeQueue -> (Maybe Value, FakeQueue)
pop m@(FakeQueue s amount id)
  | S.null s  = (Nothing, m)
  | otherwise = let (s' S.:> i) = S.viewr s
                    restMemory = FakeQueue s' amount id
                in (Just i, restMemory)
                
isEmpty :: FakeQueue -> Bool
isEmpty (FakeQueue s _ _) = S.null s

part2Algorithm :: Program -> (FakeQueue, FakeQueue)
part2Algorithm p = process p
                            ((FakeQueue S.empty 0 1, FakeQueue S.empty 0 2),
                             (M.singleton 'p' 0, M.singleton 'p' 1),
                             (0, 0),
                             (False, False))

process :: Program -> ((FakeQueue, FakeQueue), (Memory, Memory), (Index, Index), (IsLocked, IsLocked))
--                    -> ((FakeQueue, FakeQueue), (Memory, Memory), (Index, Index), (IsLocked, IsLocked))
                    -> (FakeQueue, FakeQueue)
process program ((activeInput, activeOutput), (activeMem, inactiveMem), (activeIndex, inactiveIndex), (activeIsLocked, inactiveIsLocked)) =
  if activeIsLocked && inactiveIsLocked
     then (activeInput, activeOutput)
     else let (newActiveMem, newActiveInput, newActiveOutput, newActiveIndex, newActiveIsLocked) =
                 part2Update program (activeMem, activeInput, activeOutput, activeIndex)
          in process program ((newActiveOutput, newActiveInput),
                               (inactiveMem, newActiveMem),
                               (inactiveIndex, newActiveIndex),
                               (inactiveIsLocked, newActiveIsLocked))
          
part2Update :: Program -> (Memory, FakeQueue, FakeQueue, Index) -> (Memory, FakeQueue, FakeQueue, Index, IsLocked)
part2Update program (memory, input, output, index) = case program V.! index of
  Send       (getValue -> n) -> (memory, input, enqueue n output, 1+index, False)
  Set      r (getValue -> n) -> (M.insert r n memory, input, output, 1+index, False)
  Add      r (getValue -> n) -> (updateValue r (+) n, input, output, 1+index, False)
  Multiply r (getValue -> n) -> (updateValue r (*) n, input, output, 1+index, False)
  Mod      r (getValue -> n) -> (updateValue r mod n, input, output, 1+index, False)
  Receive  r                 -> if isEmpty input
                                  then (memory, input, output, index, True)
                                  else let (Just e, newInput) = pop input
                                       in (M.insert r e memory, newInput, output, 1+index, False)
  JumpGreaterThan0
    (getValue -> check)
    (getValue -> n)          -> if check > 0
                                  then (memory, input, output, index + fromIntegral n, False)
                                  else (memory, input, output, 1+index, False)
 where
  getValue (Literal n) = n
  getValue (Pointer register) = M.findWithDefault 0 register memory
    
  updateValue reg op n = M.insert reg (M.findWithDefault 0 reg memory `op` n) memory

------------------------------------------------------------------

inputForPart1 :: IO Program
inputForPart1 =
  let filename = "src/input_day18.txt"
  in processEither . parse parseFile filename <$> readFile filename
 where
  parseFile = V.fromList <$> parseInstruction `sepBy` eol
  parseInstruction = parseSend <|>
               parseSet <|>
               parseAdd <|>
               parseMultiply <|>
               parseMod <|>
               parseRecover <|>
               parseJumpGreaterThan
  parseRecover = Recover <$> (string "rcv " *> parseValueSource)

inputForPart2 :: IO Program
inputForPart2 =
  let filename = "src/input_day18.txt"
  in processEither . parse parseFile filename <$> readFile filename
 where
  parseFile = V.fromList <$> parseInstruction `sepBy` eol
  parseInstruction = parseSend <|>
               parseSet <|>
               parseAdd <|>
               parseMultiply <|>
               parseMod <|>
               parseReceive <|>
               parseJumpGreaterThan
  parseReceive = Receive <$> (string "rcv " *> lowerChar)
  
    
processEither (Left  e ) = error (parseErrorPretty e)
processEither (Right rs) = rs
  
parseSend = Send <$> (string "snd " *> parseValueSource)
parseSet = parseRegisterValue "set" Set
parseAdd = parseRegisterValue "add" Add
parseMultiply = parseRegisterValue "mul" Multiply
parseMod = parseRegisterValue "mod" Mod

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
  [ testCase "part 1" $ part1 >>= (@?= 3188)
  , testCase "part 2" $ part2 >>= (@?= 7112)
  ]

