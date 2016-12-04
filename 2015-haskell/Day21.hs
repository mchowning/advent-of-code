{-# OPTIONS_GHC -Wall -Werror #-}

module Day21 where

import Data.List
import Data.Maybe
import Control.Monad(replicateM)
import Test.HUnit

type HitPts = Int
type DamagePts = Int
type ArmorPts = Int
type Weapon = Item
type Armor = Item
type Ring = Item

data Item = Item { iName :: String
                 , iCost :: Int
                 , iDamage :: DamagePts
                 , iArmor :: ArmorPts
                 } deriving (Eq, Ord)

instance Show Item where show i = "{ " ++ iName i ++ " }"


data Player = Player { pName :: String
                     , hitPoints :: HitPts
                     , pDamage :: DamagePts
                     , pArmor :: ArmorPts
                     , equipmentCost :: Int
                     } deriving (Show)

instance Eq Player where p1 == p2 = pName p1 == pName p2


-- 111
result1 :: Int
result1 = minimum . map equipmentCost . filter wouldWinBattle $ getPlayers

-- 188
result2 :: Int
result2 = maximum . map equipmentCost . filter (not . wouldWinBattle) $ getPlayers

input :: String
input = "Hit Points: 109\n\
        \Damage: 8\n\
        \Armor: 2"
opponent :: Player
opponent = Player "opponent" 109 8 2 0

getPlayers :: [Player]
getPlayers = map getPlayer allEquipmentOptions
  where
    getPlayer :: [Item] -> Player
    getPlayer ls = let d = calcAttr iDamage 
                       a = calcAttr iArmor
                       c = calcAttr iCost
                   in Player "player" 100 d a c
      where
        calcAttr :: (Item -> Int) -> Int
        calcAttr f = sum . map f $ ls

allEquipmentOptions :: [[Item]]
allEquipmentOptions = map catMaybes allEquipmentOptions'
  where 
    allEquipmentOptions' :: [[Maybe Item]]
    allEquipmentOptions' = [ w:a:rs | w <- weaponOptions, a <- armorOptions, rs <- ringOptions]

battle :: Player -> Player -> Player
battle (Player _ 0 _ _ _) p = p 
battle p1@(Player _ _ d1 _ _) p2@(Player _ h2 _ a2 _) =
  let damage = max 1 (d1 - a2)
      newP2 = p2 { hitPoints = max 0 (h2 - damage) }
  in battle newP2 p1

wouldWinBattle :: Player -> Bool
wouldWinBattle p = p == battle p opponent

weaponOptions :: [Maybe Item]
weaponOptions = map Just weaponList
  where 
    weaponList :: [Item]
    weaponList = [ Item "Dagger"     8 4 0
                 , Item "Shortsword" 10 5 0
                 , Item "Warhammer"  25 6 0
                 , Item "Longsword"  40 7 0
                 , Item "Greataxee"  74 8 0 ]

armorOptions :: [Maybe Item]
armorOptions = Nothing : map Just armorList
  where
    armorList :: [Item]
    armorList = [ Item "Leather"    13  0 1
                , Item "Chainmail"  31  0 2
                , Item "SplintMail" 53  0 3
                , Item "Bandedmail" 75  0 4
                , Item "Platemail"  102 0 5 ]

ringOptions :: [[Maybe Item]]
ringOptions = let allCombos = replicateM 2 (Nothing : map Just ringList) 
              in filter (\[x,y] -> x /= y) . nub . map sort $ allCombos
  where
    ringList :: [Item]
    ringList = [ Item "Damage +1"  25  1 0
               , Item "Damage +2"  50  2 0
               , Item "Damage +3"  100 3 0
               , Item "Defense +1" 20  0 1
               , Item "Defense +2" 40  0 2
               , Item "Defense +3" 80  0 3 ]


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ length ringOptions ~?= 21
  , nub (map length ringOptions)       ~?= [2]  -- all ringOptions are lists of length 2
  , all (\[x,y] -> x /= y) ringOptions ~?= True -- never have 2 of same thing

  , pName (battle (Player "p1" 8 5 5 0) (Player "" 12 7 2 0)) ~?= "p1"

  , result1 ~?= 111
  , result2 ~?= 188
  ]
