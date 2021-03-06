{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}

module Day8 where

import qualified Data.Map.Strict  as M
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Turtle.Pattern   (alphaNum, anyChar, decimal, match, plus,
                                   signed, space)

import           Test.Tasty
import           Test.Tasty.HUnit

type Register = T.Text

data Mod
  = Increment
  | Decrement
  deriving (Show, Eq)

data Update = Update
  { updateReg :: Register
  , updateMod :: Mod
  , updateN   :: Int
  } deriving (Show, Eq)

data Comparator
  = Less
  | LessEqual
  | Equal
  | NotEqual
  | GreaterEqual
  | Greater
  deriving (Show, Eq)

data Condition = Condition
  { conditionReg  :: Register
  , conditionComp :: Comparator
  , conditionN    :: Int
  } deriving (Show, Eq)

data Instruction = Instruction
  { update    :: Update
  , condition :: Condition
  } deriving (Show, Eq)

input :: IO [Instruction]
input = parseInstructions <$> TIO.readFile "src/input_day8.txt"

part1 :: IO Int
part1 = part1Algo <$> input

part1Algo :: [Instruction] -> Int
part1Algo = foldr max (minBound :: Int) . updateAllValues

part2 :: IO Int
part2 = part2Algo <$> input

part2Algo :: [Instruction] -> Int
part2Algo ls = (M.!) (foldl processInstructionWithMax M.empty ls) "max"

updateAllValues :: [Instruction] -> M.Map T.Text Int
updateAllValues = foldl processInstruction M.empty

processInstructionWithMax :: M.Map T.Text Int -> Instruction -> M.Map T.Text Int
processInstructionWithMax m (Instruction u c) =
  if processCondition m c
    then let updatedMap = processUpdate m u
             thisValue = M.findWithDefault 0 (updateReg u) updatedMap
             prevMax = M.findWithDefault 0 "max" updatedMap
         in M.insert "max" (max prevMax thisValue) updatedMap
    else m

processInstruction :: M.Map T.Text Int -> Instruction -> M.Map T.Text Int
processInstruction m (Instruction u c) =
  if processCondition m c
    then processUpdate m u
    else m

processCondition :: M.Map T.Text Int -> Condition -> Bool
processCondition m (Condition k c n) = comparatorFor c (M.findWithDefault 0 k m) n

comparatorFor :: Comparator -> (Int -> Int -> Bool)
comparatorFor =
  \case
    Less -> (<)
    LessEqual -> (<=)
    Equal -> (==)
    NotEqual -> (/=)
    GreaterEqual -> (>=)
    Greater -> (>)

processUpdate :: M.Map T.Text Int -> Update -> M.Map T.Text Int
processUpdate m (Update k modifier n) =
  let currentValue = M.findWithDefault 0 k m
      newValue = updateValue currentValue n modifier
  in M.insert k newValue m

updateValue :: Int -> Int -> Mod -> Int
updateValue a b m =
  let f =
        case m of
          Increment -> (+)
          Decrement -> (-)
  in f a b

parseInstruction :: T.Text -> Instruction
parseInstruction = head . match instructionPattern
  where
    instructionPattern = do
      u <- updatePattern
      cond <- conditionPattern
      return $ Instruction u cond
    updatePattern = do
      reg <- plus alphaNum <* space
      md <- plus alphaNum <* space
      n <- signed decimal <* space
      return $ Update reg (modMatch md) n
    conditionPattern = do
      reg <- "if " *> plus alphaNum <* space
      comp <- plus anyChar <* space
      n <- signed decimal
      return $ Condition reg (comparatorOf comp) n
    modMatch =
      \case
        "inc" -> Increment
        "dec" -> Decrement
        p -> error ("invalid modPattern: " ++ show p)
    comparatorOf =
      \case
        "<" -> Less
        "<=" -> LessEqual
        "==" -> Equal
        "!=" -> NotEqual
        ">=" -> GreaterEqual
        ">" -> Greater
        p -> error ("invalid modPattern: " ++ show p)

parseInstructions :: T.Text -> [Instruction]
parseInstructions = map parseInstruction . T.lines

--------------------------------------------------------------------------------
sampleInput :: T.Text
sampleInput =
  "b inc 5 if a > 1\n\
  \a inc 1 if b < 5\n\
  \c dec -10 if a >= 1\n\
  \c inc -20 if c == 10"

tests :: IO ()
tests = defaultMain $ testGroup "Day 8 Tests"
  [ parseTests
  , processConditionTest
  , processInstructionTest
  , part1AlgoTests
  , part2AlgoTests
  ]
  where
    parseTests =
      testGroup
        "parseInstruction"
        [ testCase "successful parse" $
          parseInstruction "b inc 5 if a > 1" @?= Instruction (Update "b" Increment 5) (Condition "a" Greater 1)
        , testCase "parse negative adjustment" $
          parseInstruction "b dec -5 if a == 1" @?= Instruction (Update "b" Decrement (-5)) (Condition "a" Equal 1)
        ]
    processConditionTest =
      testCase "processCondition" $ processCondition M.empty (Condition "a" GreaterEqual 0) @? "should be true"
    processInstructionTest =
      testCase "processInstruction" $ updateAllValues (parseInstructions sampleInput) M.! "c" @?= -10
    part1AlgoTests =
      testGroup
        "part 1 algorithm"
        [ testCase "sample input" $ part1Algo (parseInstructions sampleInput) @?= 1
        , testCase "real input" $ do
            instructions <- input
            part1Algo instructions @?= 4163
        ]
    part2AlgoTests = testGroup "part 2 algorithm"
      [ testCase "sample input" $ part2Algo (parseInstructions sampleInput) @?= 10
      , testCase "real input" $ do
          instructions <- input
          part2Algo instructions @?= 5347
      ]
