{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import           Util

import Data.Maybe (fromMaybe)
import           Control.Monad              (void)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import           Debug.Trace

type ChemicalAmount = (String, Int)
type Reaction = ([ChemicalAmount], ChemicalAmount)
type ReactionMap = M.Map String Reaction
type Inventory = M.Map String Int
type WithReactionMap = Reader ReactionMap

part1 :: IO Int
part1 = part1' <$> input

part1' :: ReactionMap -> Int
part1' = runReader (oreFor ("FUEL", 1))

oreFor :: ChemicalAmount -- desired
       -> WithReactionMap Int -- quantity of ore needed to produce desired
oreFor desired = do
  produceResult <- produce mempty desired
  let oreAmount = produceResult M.!? "ORE"
  return (fromMaybe (error "produce resulted in no ore") oreAmount)

produce :: Inventory -- available inventory
        -> ChemicalAmount -- desired
        -> WithReactionMap Inventory -- inventory updated to have desired
produce inventory ("ORE", n) = return (M.insertWith (+) "ORE" n inventory)
produce inventory (desiredName, desiredAmount) =
  let available = M.findWithDefault 0 desiredName inventory
  in if available >= desiredAmount
       then return (M.insert desiredName (available - desiredAmount) inventory)
       else do
         mReaction <- asks (M.lookup desiredName)
         let desiredReaction = fromMaybe
                                 (error $ "Could not find reaction that creates " <> desiredName)
                                 mReaction
             additionalNeeded = desiredAmount - available
             (newChems, (_, newAmount)) = minimumIngredients desiredReaction additionalNeeded
             newInventory = M.insert desiredName (newAmount - additionalNeeded) inventory
         foldM produce newInventory newChems

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
part2' = runReader (fuelFromOre 1000000000000)

fuelFromOre :: Int -> WithReactionMap Int
fuelFromOre numOre = do
  rm <- ask
  let oreForNFuel n = runReader (oreFor ("FUEL", n)) rm
      inventoryExceedsConsumption = (< numOre) . oreForNFuel
  return (binarySearchBiggestWhich 0 numOre inventoryExceedsConsumption)

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

