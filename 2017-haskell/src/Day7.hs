{-# LANGUAGE OverloadedStrings #-}
module Day7 (part1, part2) where

import           Data.List        (all, group, sort, sortBy, zip)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (listToMaybe, mapMaybe)
import qualified Data.Set         as S
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Turtle.Pattern   (Pattern, alphaNum, anyChar, between, digit,
                                   match, option, plus, sepBy1,decimal)

type Name = T.Text
data Program = Program { name         :: Name
                       , heldPrograms :: [Name]
                       , number       :: Int
                       } deriving (Show, Eq)

data ProgramTree = Node Program Int [ProgramTree]
                 | Leaf Program Int
                 deriving (Eq, Show)

data Weights = Weights Int [Int]
             deriving (Show, Eq)

input :: IO [Program]
input = parsePrograms <$> TIO.readFile "src/input_day7.txt"

part1 :: IO T.Text
part1 = name . getRootProgram <$> input

part2 :: IO T.Text
part2 = T.pack . show . part2Algo <$> input

part2Algo :: [Program] -> Int
part2Algo ps=
  let levelWithImbalance = findLevelWithImbalance ps
      [outlier, goal] = sortByFrequency . map getWeight $ levelWithImbalance
      nodeToAdjust = withWeight outlier levelWithImbalance 
  in number (getProgram nodeToAdjust) + (goal - outlier)
    where
      withWeight w = head . filter ((== w) . getWeight)
      sortByFrequency = map head . sortBy (\a b -> compare (length a) (length b)) . group . sort

createTree :: [Program] -> ProgramTree
createTree allPs =
  let pMap = M.fromList $ zip (map name allPs) allPs
      root = getRootProgram allPs
  in createTree' pMap root

createTree' :: M.Map Name Program-> Program -> ProgramTree
createTree' pMap root =
  if null (heldPrograms root)
    then Leaf root (number root)
    else let children = map (createTree' pMap . (pMap M.!)) (heldPrograms root)
             weight = number root + sum (map getWeight children)
         in Node root weight children


findLevelWithImbalance :: [Program] -> [ProgramTree]
findLevelWithImbalance ps = findLevelWithImbalance' $ getChildren (createTree ps)

findLevelWithImbalance' :: [ProgramTree] -> [ProgramTree]
findLevelWithImbalance' ps =
  let imbalancedTrees = withImbalancedChildren ps
  in if null imbalancedTrees
       then ps
       else if length imbalancedTrees > 1
              then error ("too many imbalanced trees " ++ show (length imbalancedTrees))
              else findLevelWithImbalance' . getChildren . head $ imbalancedTrees

withImbalancedChildren :: [ProgramTree] -> [ProgramTree]
withImbalancedChildren = filter (not . isBalanced)
  where
    isBalanced = allTheSame . map getWeight . getChildren

getProgram :: ProgramTree -> Program
getProgram (Node p _ _) = p
getProgram (Leaf p _)   = p

allTheSame :: [Int] -> Bool
allTheSame xs = all (== head xs) (tail xs)

getChildren :: ProgramTree -> [ProgramTree]
getChildren (Node _ _ cs) = cs
getChildren _             = []

getWeight :: ProgramTree -> Int
getWeight (Node _ w _) = w
getWeight (Leaf _ w)   = w

getRootProgram :: [Program] -> Program
getRootProgram ps = let allChildren = S.fromList (concatMap heldPrograms ps)
                    in head . filter (flip S.notMember allChildren . name) $ ps

parsePrograms :: T.Text -> [Program]
parsePrograms = mapMaybe parseProgram . T.lines


parseProgram :: T.Text -> Maybe Program
parseProgram = listToMaybe . match programPattern

programPattern :: Pattern Program
programPattern = do
  n <- plus anyChar <* " "
  d <- between "(" ")" decimal
  cs <- option " -> " *>
        option (plus alphaNum `sepBy1` ", ")
  return (Program n cs d)

---------------------------------------------------------------------------------

tests :: IO ()
tests = defaultMain $ testGroup "Day 7 Tests"
  [ parseProgramTests
  , getRootProgramTests
  , part2AlgoTests
  ]
  where
    sampleInput = "pbga (66)\n\
                  \xhth (57)\n\
                  \ebii (61)\n\
                  \havc (66)\n\
                  \ktlj (57)\n\
                  \fwft (72) -> ktlj, cntj, xhth\n\
                  \qoyq (66)\n\
                  \padx (45) -> pbga, havc, qoyq\n\
                  \tknk (41) -> ugml, padx, fwft\n\
                  \jptl (61)\n\
                  \ugml (68) -> gyxo, ebii, jptl\n\
                  \gyxo (61)\n\
                  \cntj (57)"
    parseProgramTests = testGroup "can parse Programs"
      [ testCase "typical" $
          parseProgram  "name (123) -> child1, child2" @?=
            Just (Program "name" ["child1", "child2"] 123)
      , testCase "without children" $
          parseProgram  "name (123)" @?=
            Just (Program "name" [] 123)
      , testCase "handles failed parse" $
          parseProgram  "invalid input" @?= Nothing ]
    getRootProgramTests = testGroup "part 1 algorithm"
      [ testCase "sample input" $
          (name . getRootProgram . parsePrograms $ sampleInput) @?= "tknk"
      , testCase "actual input" $ do
          content <- input
          name (getRootProgram content) @?= "dgoocsw"
      ]
    part2AlgoTests = testGroup "part 2 algorithm"
      [ testCase "sample input" $ (part2Algo . parsePrograms $ sampleInput) @?= 60
      , testCase "actual input" $ do
          content <- input
          part2Algo content @?= 1275
      ]
