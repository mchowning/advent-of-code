{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Util

import Control.Monad (void)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import Debug.Trace

type ChemicalAmount = (String, Int)

type Reaction = ([ChemicalAmount], ChemicalAmount)

type ReactionMap = M.Map String Reaction

type Inventory = M.Map String Int

part1 :: IO Int
part1 = part1' <$> input

part1' :: ReactionMap -> Int
part1' rm = oreFor rm ("FUEL", 1)

oreFor :: ReactionMap -> ChemicalAmount -> Int
oreFor rm desired =
  case produce rm mempty desired M.!? "ORE" of
    Just n -> n
    Nothing -> error "produce resulted in no ore"


produce :: ReactionMap -- possible reactions
        -> Inventory -- available inventory
        -> ChemicalAmount -- desired
        -> Inventory -- inventory updated to have desired
produce _ inventory ("ORE", n) = M.insertWith (+) "ORE" n inventory
produce reactionMap inventory (desiredName, desiredAmount) =
  let available = M.findWithDefault 0 desiredName inventory
  in if available >= desiredAmount
       then M.insert desiredName (available - desiredAmount) inventory
       else
         let additionalNeeded = desiredAmount - available
             reactionToGetMore = case reactionMap M.!? desiredName of
                                   Just r -> r
                                   Nothing -> error ("Could not find reaction that creates " <> desiredName)
             (newChems, (_, newAmount)) = minimumIngredients reactionToGetMore additionalNeeded
             newInventory = M.insert desiredName (newAmount - additionalNeeded) inventory
         in foldl' (produce reactionMap) newInventory newChems

minimumIngredients :: Reaction -- reaction to use
                   -> Int  -- minmimum desired output quantity
                   -> ([ChemicalAmount], ChemicalAmount) -- reaction multiplied by number of reactions needed
minimumIngredients (rInputs, output@(outputName, outputNum)) desiredAmount =
  let numReactionsNeeded = (1 + ((desiredAmount - 1) `div` outputNum))
      ingredients = (fmap . fmap) (* numReactionsNeeded) rInputs
      resultingOutput = fmap (* numReactionsNeeded) output
  in (ingredients, resultingOutput)


------------------------------------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> input

part2' :: ReactionMap -> Int
part2' = fuelFromOre 1000000000000

fuelFromOre :: Int -> ReactionMap -> Int
fuelFromOre numOre rm = binarySearchBiggestWhich 0 numOre (\i -> oreFor rm ("FUEL", i) < numOre)

binarySearchBiggestWhich :: Int
                         -> Int
                         -> (Int -> Bool)
                         -> Int
binarySearchBiggestWhich lower upper pred
  | lower == upper = if pred lower 
                       then lower 
                       else error "search localized to value that failed predicate"
  | otherwise =
      let mid = (lower + upper + 1) `div` 2 -- add 1 to make sure it rounds toward upper
      in if pred mid
           then binarySearchBiggestWhich mid upper pred
           else binarySearchBiggestWhich lower (mid - 1) pred

------------------------------------------------------------------------------------------


input :: IO ReactionMap
input = parseInput parser "day14.txt"

mkReactionMap :: [Reaction] -> ReactionMap
mkReactionMap rs = M.fromList [ (outputName, reaction) | reaction <- rs, let outputName = fst (snd reaction)]

parser :: Parser ReactionMap
parser = mkReactionMap <$> reactionParser `sepBy1` eol

reactionParser :: Parser Reaction
reactionParser = do
  ingredients <- ingredientParser `sepBy1` string ", "
  void (string " => ")
  output <- ingredientParser
  return (ingredients, output)

ingredientParser :: Parser ChemicalAmount
ingredientParser = do
  num <- decimal
  space1
  name <- some letterChar
  return (name, num)

