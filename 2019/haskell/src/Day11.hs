{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Day11 where

import           Util

import           Day5                (readInputFrom)

import           Control.Monad       (forM_)
import           Data.List           (groupBy, permutations)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import           Prelude             hiding (Left, Right)

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: Program -> Int
part1' = M.size . startComputer 0

test :: IO (M.Map (Int, Int) Int)
test = startComputer 1 <$> readInput

part2 :: IO ()
part2 = do
  input <- readInput
  displayPaint (startComputer 1 input)

displayPaint :: M.Map (Int, Int) Int -> IO ()
displayPaint m =
  let points = M.keys m
      xs = fst <$> points
      ys = snd <$> points
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in forM_ [minY..maxY] $ \y ->
       let colors = (\x -> m M.!? (x,y)) <$> [minX..maxX]
           printColor = \case
             Just 0 -> "  "
             Just 1 -> "▓▓"
             Nothing -> "░░"
           printable = concatMap printColor colors
       in putStrLn printable


-----------------------------

type Index = Int
type Program = [Integer]

data Direction = Up
               | Right
               | Down
               | Left
               deriving (Eq, Show)

turnLeft :: Direction -> Direction
turnLeft = \case
  Up -> Left
  Right -> Up
  Down -> Right
  Left -> Down

turnRight :: Direction -> Direction
turnRight = turnLeft . turnLeft . turnLeft

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

type PhaseSetting = Int

startComputer :: Integer -> Program -> M.Map (Int, Int) Int
startComputer startColor program = fst (last cameraReadings)
  where
    zeros = 0:zeros
    colorReadings = catMaybes (fmap fromIntegral . snd <$> cameraReadings)
    movePaintInstructions = runComputer Computer { inputs = startColor:colorReadings
                                                 , index = 0
                                                 , relativeBase = 0
                                                 , program = program ++ zeros }
    startPoint = (0,0)
    cameraReadings = updateMap M.empty (fromIntegral <$> movePaintInstructions) startPoint Up

updateMap :: M.Map (Int, Int) Int -- coordinate color map
          -> [Int]                -- inputs
          -> (Int, Int)           -- currentLocation
          -> Direction
          -> [(M.Map (Int, Int) Int, Maybe Int)]
updateMap paintedPoints (i1:i2:is) coord direction =
  let newMap = if i1 `elem` [0,1]
                     then M.insert coord i1 paintedPoints
                     else error ("unexpected first input (paint) in input pair: " <> show i1)

      newDirection = case i2 of
               0 -> turnLeft direction
               1 -> turnRight direction
               n -> error ("unexpected second input (turn) in input pair: " <> show n)

      newCoord = moveForward newDirection coord

      colorOfNewLocation = case newMap M.!? newCoord of
                             (Just color) -> color
                             Nothing      -> 0

  in (newMap, Just colorOfNewLocation) : updateMap newMap is newCoord newDirection

updateMap m [] _ _ = [(m, Nothing)]
updateMap _ [x] _ _ = error "only got 1 instruction"

moveForward :: Direction -> (Int, Int) -> (Int, Int)
moveForward dir (x,y) =
  case dir of
    Up    -> (x, y-1)
    Down  -> (x, y+1)
    Right -> (x+1, y)
    Left  -> (x-1, y)


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

-----------------------------------------------------------------------------

readInput :: IO [Integer]
readInput = fmap fromIntegral . V.toList <$> readInputFrom "day11.txt"
