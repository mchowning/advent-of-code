{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day18 (part1, part2) where

import qualified Data.Map.Strict            as M
import qualified Data.Sequence              as S
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec            (Parsec, parse, parseErrorPretty,
                                             sepBy, (<|>))
import           Text.Megaparsec.Char       (eol, lowerChar, space1, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

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

type ValueSource = Either Register Value

data Instruction = Send             ValueSource
                 | BinOp            (Value -> Value -> Value) Register ValueSource
                 | Recover          ValueSource --part1
                 | Receive          Register    --part2
                 | JumpGreaterThan0 ValueSource ValueSource

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
  BinOp op r (getValue -> n) -> let newValue = M.findWithDefault 0 r m `op` n
                                    updatedMap = M.insert r newValue m
                                in (ss, updatedMap, i+1, False)
  Recover    (getValue -> n) -> (ss, m, i+1, n /= 0)
  JumpGreaterThan0
    (getValue -> check)
    (getValue -> n)          -> if check > 0
                                  then (ss, m, i + fromIntegral n, False)
                                  else (ss, m, i+1, False)
  Receive _                  -> error "invalid part 1 program: Receive" -- FIXME, shouldn't have to do this
 where
   getValue (Right n)        = n
   getValue (Left register) = M.findWithDefault 0 register m

enqueue :: Value -> FakeQueue -> FakeQueue
enqueue v (FakeQueue s amount ident) = FakeQueue (v S.<| s) (1+amount) ident

pop :: FakeQueue -> (Maybe Value, FakeQueue)
pop m@(FakeQueue s amount ident)
  | S.null s  = (Nothing, m)
  | otherwise = let (s' S.:> i) = S.viewr s
                    restMemory = FakeQueue s' amount ident
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
  BinOp op r (getValue -> n) -> let newValue = M.findWithDefault 0 r memory `op` n
                                    updatedMap = M.insert r newValue memory
                                in (updatedMap, input, output, 1+index, False)
  Receive  r                 -> if isEmpty input
                                  then (memory, input, output, index, True)
                                  else let (Just e, newInput) = pop input
                                       in (M.insert r e memory, newInput, output, 1+index, False)
  JumpGreaterThan0
    (getValue -> check)
    (getValue -> n)          -> if check > 0
                                  then (memory, input, output, index + fromIntegral n, False)
                                  else (memory, input, output, 1+index, False)
  Recover _                  -> error "invalid Program for part 2: Recover" -- FIXME shouldn't have to do this
 where
  getValue (Left register) = M.findWithDefault 0 register memory
  getValue (Right n)        = n

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
parseSet = parseRegisterValue "set" (BinOp (const id))
parseAdd = parseRegisterValue "add" (BinOp (+))
parseMultiply = parseRegisterValue "mul" (BinOp (*))
parseMod = parseRegisterValue "mod" (BinOp mod)

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

parseValueSource = Left <$> lowerChar <|> Right <$> signed mempty decimal

------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 18"
  [ testCase "part 1" $ part1 >>= (@?= 3188)
  , testCase "part 2" $ part2 >>= (@?= 7112)
  ]

