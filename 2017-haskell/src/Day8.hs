{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Map.Strict  as M
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Turtle.Pattern   (alphaNum, anyChar, decimal, match, plus,
                                   signed, space)

import           Test.Tasty
import           Test.Tasty.HUnit

-- 2964 is too low
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

updateAllValues :: [Instruction] -> M.Map T.Text Int
updateAllValues = foldl processInstruction M.empty

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
    modMatch =
      \case
        "inc" -> Increment
        "dec" -> Decrement
        p -> error ("invalid modPattern: " ++ show p)
    conditionPattern = do
      reg <- "if " *> plus alphaNum <* space
      comp <- plus anyChar <* space
      n <- signed decimal
      return $ Condition reg (comparatorOf comp) n
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
tests = defaultMain $ testGroup "Day 8 Tests" [parseTests, processConditionTest, processInstructionTest, part1AlgoTest]
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
    part1AlgoTest = testCase "part 1 algorithm" $ part1Algo (parseInstructions sampleInput) @?= 1
