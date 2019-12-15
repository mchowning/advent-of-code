{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Util

--import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad (guard, void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import Debug.Trace

data Ingredient = Ingredient
                { name :: String
                , quantity :: Integer
                } deriving Show

newtype IngredientCollection = IngredientCollection (M.Map String Integer)
                             deriving (Eq, Monoid, Ord, Semigroup, Show)

ingredients :: IngredientCollection -> [Ingredient]
ingredients (IngredientCollection m) = uncurry Ingredient <$> M.toList m

data Reaction = Reaction { inputs :: IngredientCollection
                         , output :: Ingredient
                         } deriving Show

mkIngredientCollection :: [Ingredient] -> IngredientCollection
mkIngredientCollection = foldl' (flip addIngredient) mempty

removeIngredient :: Ingredient -> IngredientCollection -> Maybe IngredientCollection
removeIngredient Ingredient {..} (IngredientCollection allAvailable) =
    let newValue = fromMaybe 0 (allAvailable M.!? name) - quantity
    in if | newValue > 0 -> Just . IngredientCollection $ M.insert name newValue allAvailable
          | newValue == 0 -> Just . IngredientCollection $ M.delete name allAvailable
          | newValue < 0 -> Nothing


addIngredient :: Ingredient -> IngredientCollection -> IngredientCollection
addIngredient Ingredient {..} (IngredientCollection coll) =
  IngredientCollection (M.insertWith (+) name quantity coll)

reverseReaction :: Reaction  -> IngredientCollection -> Maybe IngredientCollection
reverseReaction Reaction {..} ic =
  let withoutOutput = removeIngredient output ic
      withInput = foldr (fmap . addIngredient) withoutOutput (ingredients inputs)
  in withInput

performReaction :: Reaction -> IngredientCollection -> Maybe IngredientCollection
performReaction Reaction {..} ic =
  let withoutInputs = foldr (\i mic -> removeIngredient i =<< mic) (Just ic) (ingredients inputs)
      withOuptut = addIngredient output <$> withoutInputs
  in withOuptut

amountRequired :: String -> [Reaction] -> IngredientCollection -> [IngredientCollection]
amountRequired what rs (IngredientCollection m)
  | null m = [mempty]
  | otherwise =
    let amountOfWhat = fromMaybe 0 (M.lookup what m)
        rest = M.delete what m
        reducedRest = catMaybes (flip reverseReaction (IngredientCollection rest) <$> rs)
        result = if | null rest ->
                        [IngredientCollection m] -- SUCCESS! We've reduced down to just what we're seeking
                    | null reducedRest ->
                        [] -- FAILURE! We have other stuff, but it can't be reduced, so fail
                    | otherwise ->
                        addIngredient (Ingredient what amountOfWhat) <$> concatMap (amountRequired what rs) reducedRest
    in result

consumeAndRun1 :: [Reaction] -- available reactions
              -> IngredientCollection -- available ingredients
              -> [Ingredient] -- lazy stream of ingredients to consume
              -> (IngredientCollection -> Bool) -- predicate to stop consumption
              -> S.Set IngredientCollection -- already processed
              -> [(IngredientCollection, IngredientCollection)] -- consumed ingredients and resulting output
consumeAndRun1 rs available (i:is) pred s =
  let (resultsFromThisInput, s') = consumeAndRun' rs [(available, available)] pred s
      newInput = addIngredient i available
--  in trace (show . S.size $ s') $ resultsFromThisInput ++ consumeAndRun1 rs newInput is pred s'
  in resultsFromThisInput ++ consumeAndRun1 rs newInput is pred s

consumeAndRun' :: [Reaction] -- available reactions
              -> [(IngredientCollection, IngredientCollection)] -- consumed and resulting ingredients to process
              -> (IngredientCollection -> Bool) -- predicate to stop consumption
              -> S.Set IngredientCollection -- collections already queued
              -> ([(IngredientCollection, IngredientCollection)], S.Set IngredientCollection) -- consumed ingredients and resulting output
consumeAndRun' rs [] _ s = ([], s)
consumeAndRun' rs ((consumed, resulting):crs) pred s
  | S.member resulting s = ([], s)
  | pred resulting = ([(consumed, resulting)], (S.insert resulting s))
  | otherwise =
    let afterReactions = catMaybes (flip performReaction resulting <$> rs)
        (plainResults, s') = consumeAndRun' rs crs pred (S.insert resulting s)
        (afterResults, s'') = consumeAndRun' rs ((consumed, ) <$> afterReactions) pred s'
    in (plainResults ++ afterResults, s'')

infiniteOre :: [Ingredient]
infiniteOre = Ingredient "ORE" 1 : infiniteOre

infiniteOre' :: Int -> [Ingredient]
infiniteOre' n = Ingredient "ORE" 1 : infiniteOre' (n+1)
--infiniteOre' n = trace (show n) $ Ingredient "ORE" 1 : infiniteOre' (n+1)

------------------------------------------------------------------------------------------

fillRequirement :: S.Set IngredientCollection -> [Reaction] -> IngredientCollection -> (IngredientCollection -> Bool) -> [IngredientCollection]
fillRequirement s rs i@(IngredientCollection m) pred
  | pred i = [i]
  | S.member i s = []
  | otherwise =
      let remainingRequirements = M.filter (> 0) m
          matchingReactions = filter ((`M.member` remainingRequirements) . name . output) rs
      in if null matchingReactions
           then trace ("could not find any matching reactions with " <> show m) []
           else let newRequirements = flip reverseReaction' i <$> matchingReactions
                    filteredRequirements = filter (not . flip S.member s) newRequirements
                in concatMap (\newReq -> fillRequirement (S.insert i s) rs newReq pred) filteredRequirements

reverseReaction' :: Reaction
                 -> IngredientCollection -- requirements
                 -> IngredientCollection -- remaining requirements after reaction
reverseReaction' Reaction {..} ic =
  let withoutOutput = removeIngredient' output ic
      withInput = foldr addIngredient withoutOutput (ingredients inputs)
  in withInput


removeIngredient' :: Ingredient -> IngredientCollection -> IngredientCollection
removeIngredient' Ingredient {..} (IngredientCollection allAvailable) =
    let newValue = fromMaybe 0 (allAvailable M.!? name) - quantity
    -- FIXME when we buy extra we need to save that amount, BUT NOT AS A REQUIREMENT, it is an asset
    in IngredientCollection $ M.insert name newValue allAvailable

------------------------------------------------------------------------------------------

-- 431448
part1 :: IO Integer
part1 = part1' <$> input

collectionToMap :: IngredientCollection -> M.Map String Integer
collectionToMap (IngredientCollection m) = m

part1' :: [Reaction] -> Integer
part1' rs =
  let remainingIngredients (IngredientCollection m) = M.filter (> 0) m
      onlyOreRemains = (== ["ORE"]) . M.keys . remainingIngredients
      candidates = fillRequirement S.empty rs (mkIngredientCollection [Ingredient "FUEL" 1]) onlyOreRemains
      requiredOre = head (remainingIngredients <$> candidates)
  in head (remainingIngredients <$> candidates) M.! "ORE"



input :: IO [Reaction]
input = parseInput parser "day14.txt"

parser :: Parser [Reaction]
parser = reactionParser `sepBy1` eol

reactionParser :: Parser Reaction
reactionParser = do
  ingredients <- ingredientParser `sepBy1` string ", "
  void (string " => ")
  output <- ingredientParser
  return $ Reaction (mkIngredientCollection ingredients) output

ingredientParser :: Parser Ingredient
ingredientParser = do
  num <- decimal
  space1
  name <- some letterChar
  return (Ingredient name num)

