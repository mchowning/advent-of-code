{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day25 (part1, part2) where

import qualified Data.Set             as S
import qualified Data.Vector          as V

import           Control.Applicative  (some)
import           Data.Char            (ord)
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, count, parse, parseErrorPretty,
                                       sepBy1, (<|>))
import           Text.Megaparsec.Char (char, digitChar, eol, satisfy, space,
                                       string, upperChar)
                                       
import Test.Tasty
import Test.Tasty.HUnit

type Index = Int
newtype ChecksumIndex =
  ChecksumIndex Index
  deriving (Show)
data Tape =
  Tape Index (S.Set Index)
  deriving (Show)

data SlotValue
  = Zero
  | One
  deriving (Show)

data Move
  = Down
  | Up
  deriving (Show, Eq)

type RuleId = Int

data Rule
  = Rule SlotValue Move RuleId
  deriving (Show)

data StateRule
  = StateRule { _zero :: Rule
              , _one  :: Rule
              } deriving (Show)

part1, part2 :: IO Int
part1 = checksum <$> input
part2 = undefined

checksum :: (RuleId, ChecksumIndex, V.Vector StateRule) -> Int
checksum (ruleId, ChecksumIndex csIndex, rules) =
  let (_, _, setOfOnes) = iterate (go rules) (0, ruleId, S.empty) !! csIndex
  in S.size setOfOnes

go :: V.Vector StateRule -> (Index, RuleId, S.Set Index) -> (Index, RuleId, S.Set Index)
go rules (index, ruleId, set) =
  let stateRule = rules V.! ruleId
      rule = if S.notMember index set then _zero stateRule else _one stateRule
  in updateWithRule rule index set

updateWithRule :: Rule -> Index -> S.Set Index -> (Index, RuleId, S.Set Index)
updateWithRule (Rule slotValue move ruleId) index set =
  let newSet = updateSet set index slotValue
      newIndex = if move == Down then index-1 else index +1
  in (newIndex, ruleId, newSet)

updateSet :: S.Set Index -> Index -> SlotValue -> S.Set Index
updateSet s i Zero = S.delete i s
updateSet s i One = S.insert i s


-----------------------------------------------------------------------------------------

input :: IO (RuleId, ChecksumIndex, V.Vector StateRule)
input = parseInput "src/input_day25.txt"

parseInput :: String -> IO (RuleId, ChecksumIndex, V.Vector StateRule)
parseInput filename =
  processEither . parse parseFile filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  parseFile :: Parsec Void String (RuleId, ChecksumIndex, V.Vector StateRule)
  parseFile = do
    startState <- parseStartState
    checksumIndex <- parseChecksumIndex <* eol
    stateRules <- parseStateRule `sepBy1` eol
    return (startState, checksumIndex, V.fromList stateRules)
 
  parseStartState = do
    _ <- string "Begin in state "
    c <- upperChar
    _ <- char '.' <* eol
    return (getRuleId c)

  parseChecksumIndex = do
    _ <- string "Perform a diagnostic checksum after "
    ds <- read <$> some digitChar
    _ <- string " steps." <* eol
    return (ChecksumIndex ds)

  parseStateRule = do
    _ <- string "In state " <* upperChar <* char ':' <* eol
    _ <- count 2 space <* string "If the current value is 0:" *> eol
    r1 <- parseRule
    _ <- count 2 space <* string "If the current value is 1:" <* eol
    r2 <- parseRule
    return (StateRule r1 r2)
   where

    parseRule :: Parsec Void String Rule
    parseRule = Rule <$> parseWrite <*> parseMove <*> parseNextRule
     where
      parseWrite = count 4 space *> string "- Write the value " *> parseValue <* char '.' <* eol
       where
        parseValue = do
          v <- satisfy (\c -> c == '0' || c == '1')
          return $ if v == '0' then Zero else One

      parseMove = do
        _ <- count 4 space *> string "- Move one slot to the "
        s <- string "left" <|> string "right"
        _ <- char '.' <* eol
        return $ if s == "left" then Down else Up

      parseNextRule = do
        _ <- count 4 space *> string "- Continue with state "
        c <- upperChar
        _ <- char '.' <* eol
        return (getRuleId c)

  getRuleId c = ord c - ord 'A'

--------------------------------------------------------------------------------------------

tests = defaultMain $ testGroup "Day 25"
  [ testGroup "part 1"
      [ testCase "sample input" $ checksum <$> (parseInput "src/input_day25_sample.txt") >>= (@?= 3)
--      , testCase "real input" $ checksum <$> (parseInput "src/input_day25_sample.txt") >>= (@?= 4385) -- bit slow
      ]
  ]

