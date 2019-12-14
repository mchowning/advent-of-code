{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCode (readProgramFrom, startComputer, Program) where

import Util

import Data.Text (Text)
import qualified Data.Map.Strict as M

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

type Index = Int

type Program = [Integer]

data Mode = Position
          | Immediate
          | Relative
          deriving (Eq, Show)

data Operation = Add
               | Multiply
               | Input
               | Output
               | JumpIfTrue
               | JumpIfFalse
               | LessThan
               | Equals
               | SetRelativeBase
               | Completed
               deriving (Eq, Show)

data Computer = Computer { inputs       :: [Integer]
                         , index        :: Int
                         , relativeBase :: Int
                         , program      :: Program
                         }
                         deriving (Eq, Show)

startComputer :: [Integer] -> Program -> [Integer]
startComputer inputs program =
  runComputer Computer { inputs = inputs
                       , index = 0
                       , relativeBase = 0
                       , program = program ++ zeros }
    where
      zeros = 0 : zeros

parseOpCode :: Int -> (Operation, M.Map Int Mode)
parseOpCode n =
  let op = parseOp (n `mod` 100)
      [mode1, mode2, mode3] = parseMode . ((`mod` 10) . div n) <$> [100, 1000, 10000]
      modeMap = M.fromList [(1, mode1), (2, mode2), (3, mode3)]
  in (op, modeMap)
    where
      parseOp :: Int -> Operation
      parseOp = \case
        1 -> Add
        2 -> Multiply
        3 -> Input
        4 -> Output
        5 -> JumpIfTrue
        6 -> JumpIfFalse
        7 -> LessThan
        8 -> Equals
        9 -> SetRelativeBase
        99 -> Completed
        x -> error ("unexpected op: " <> show x)

      parseMode :: Int -> Mode
      parseMode = \case
        0 -> Position
        1 -> Immediate
        2 -> Relative
        n -> error ("Unexpected mode of " <> show n)

runComputer :: Computer -> [Integer]
runComputer amp@Computer { inputs, index, relativeBase, program } =
  let (op, modeMap) = parseOpCode (fromIntegral (program !! index))

      paramRead :: Int -> Integer
      paramRead offset = case modeMap M.! offset of
        Position  -> program !!                 fromIntegral (program !! (index + offset))
        Immediate ->                                          program !! (index + offset)
        Relative  -> program !! (relativeBase + fromIntegral (program !! (index + offset)))

      paramWrite :: Int -> Integer -> Program
      paramWrite offset value =
        let dest = case modeMap M.! offset of
                      Position -> fromIntegral (program !! (index + offset))
                      Relative -> fromIntegral (program !! (index + offset)) + relativeBase
                      Immediate -> error "unexpected Immediate mode for insertion"
        -- FIXME hacky
        in take dest program ++ value : drop (dest+1) program

  in case op of

       Add ->
         let newVal = paramRead 1 + paramRead 2
             newProgram = paramWrite 3 newVal
         in  runComputer ( amp {index = index + 4, program = newProgram })

       Multiply ->
         let newVal = paramRead 1 * paramRead 2
             newProgram = paramWrite 3 newVal
         in  runComputer ( amp {index = index + 4, program = newProgram })

       Input ->
         let newProgram = paramWrite 1 (head inputs)
         in runComputer (amp {inputs = tail inputs, index = index + 2, program = newProgram})

       Output ->
         -- Enable tail-call recursion
         paramRead 1 : runComputer (amp {index = index + 2 })

       JumpIfTrue ->
         runComputer $ case paramRead 1 of
           0 -> amp { index = index + 3 }
           _ -> amp { index = fromIntegral (paramRead 2) }

       JumpIfFalse ->
         runComputer $ case paramRead 1 of
           0 -> amp { index = fromIntegral (paramRead 2) }
           _ ->  amp { index = index + 3 }

       LessThan ->
         let newVal = if paramRead 1 < paramRead 2 then 1 else 0
             newProgram = paramWrite 3 newVal
         in runComputer (amp { index = index + 4, program = newProgram })

       Equals ->
         let newVal = if paramRead 1 == paramRead 2 then 1 else 0
             newProgram = paramWrite 3 newVal
         in runComputer (amp { index = index + 4, program = newProgram })

       SetRelativeBase ->
         let newRelativeBase = relativeBase + fromIntegral (paramRead 1)
         in runComputer (amp { index = index + 2, relativeBase = newRelativeBase })

       Completed -> []

readProgramFrom :: Text -> IO Program
readProgramFrom = parseInput (signedInt `sepBy1` char ',')
  where signedInt = signed (return()) decimal

