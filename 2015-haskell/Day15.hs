{-# OPTIONS_GHC -Wall -Werror #-}

module Day15 where

import Test.HUnit
import Control.Monad(replicateM)

data Ingredient = Ingredient { capacity   :: Int
                             , durability :: Int
                             , flavor     :: Int
                             , texture    :: Int
                             , calories   :: Int
                             } deriving (Eq, Show)

parseLine :: String -> Ingredient
parseLine str = let sCapacity = read . init $ words str !! 2
                    sDurability = read . init $ words str !! 4
                    sFlavor = read . init $ words str !! 6
                    sTexture = read . init $ words str !! 8
                    sCalories = read $ words str !! 10
                in Ingredient sCapacity sDurability sFlavor sTexture sCalories

-- 21367368
result1 :: Integer
result1 = maxFlavorRecipe input 4 100

-- 1766400
result2 :: Integer
result2 = maxFlavorRecipeWithCalories input 500 4 100

maxFlavorRecipe :: String -> Int -> Int -> Integer
maxFlavorRecipe str nElements totalQuantity = 
  let recipes = allRecipes str nElements totalQuantity
  in  maximum . map recipeFlavor $ recipes

maxFlavorRecipeWithCalories :: String -> Integer -> Int -> Int -> Integer
maxFlavorRecipeWithCalories str calLimit nElements totalQuantity = 
  let recipes = allRecipes str nElements totalQuantity
      recipesWithCalories = filter ((== calLimit). recipeCalories) recipes
  in  maximum . map recipeFlavor $ recipesWithCalories

allRecipes :: String -> Int -> Int -> [[(Int,Ingredient)]]
allRecipes str nElements totalQuantity = let ingredients = map parseLine (lines str)
                                             combos = allPossibleDistributions nElements totalQuantity
                                         in map (`zip` ingredients) combos


recipeCalories :: [(Int,Ingredient)] -> Integer
recipeCalories = flip elementValue calories

recipeFlavor :: [(Int,Ingredient)] -> Integer
recipeFlavor ls = product $ map (max 0 . elementValue ls) allFlavorElements

allFlavorElements :: [Ingredient -> Int]
allFlavorElements = [ capacity
                            , durability
                            , flavor
                            , texture ]

elementValue :: [(Int,Ingredient)] -> (Ingredient -> Int) -> Integer
elementValue ls f = fromIntegral . sum . map (\(n,i) -> n * f i) $ ls

allPossibleDistributions :: Int -> Int -> [[Int]]
allPossibleDistributions nIngredients totalCapacity = filter ((== totalCapacity) . sum) allPossibleCombinations
  where allPossibleCombinations :: [[Int]]
        allPossibleCombinations = replicateM nIngredients [0..totalCapacity]

input :: String
input = "Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3\n\
        \Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3\n\
        \Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8\n\
        \Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8"


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseLine (head (lines input)) ~?= Ingredient 2     0   (-2)  0   3
  , parseLine (lines input !! 1)   ~?= Ingredient 0     5   (-3)  0   3
  , parseLine (lines input !! 2)   ~?= Ingredient 0     0    5   (-1) 8
  , parseLine (lines input !! 3)   ~?= Ingredient 0    (-1)  0    5   8
  , parseLine testInput1           ~?= Ingredient (-1) (-2)  6    3   0
  , parseLine testInput2           ~?= Ingredient 2     3   (-2) (-1) 0

  , allPossibleDistributions 1 10          ~?= [[10]]
  , length (allPossibleDistributions 2 10) ~?= 11

  , elementValue [(44,parseLine testInput1),(56,parseLine testInput2)] capacity ~?= 68

  , recipeFlavor [(44, parseLine testInput1), (56, parseLine testInput2)] ~?= 62842880
  , recipeFlavor [(100,parseLine testInput1)] ~?= 0
  ]

testInput1 :: String
testInput1 = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 0"
testInput2 :: String
testInput2 = "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 0"
