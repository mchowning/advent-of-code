{-# OPTIONS_GHC -Wall -Werror #-}

module Day6 where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Test.HUnit
import Text.Printf

data RangeInstruction = RangeInstruction ChangeType (Int,Int) (Int,Int)
  deriving (Eq, Show)

data PointInstruction = PointInstruction { changeType :: ChangeType
                                         , coord      :: (Int,Int)
                                         } deriving (Show)
instance Ord PointInstruction where
  compare = compare `on` coord
instance Eq PointInstruction where
  (==) = (==) `on` coord -- ignoring the changeType here might be unwise

data LightState = LightOn
                | LightOff
                deriving (Show, Eq)

data ChangeType = On
                | Off 
                | Toggle
                deriving (Eq, Show)

-- TODO so slow
results :: IO ()
results = do input <- readFile "day6_input.txt"
             let numLightsOn = filter (== LightOn) . getLightOnOffStates $ input
             printResult 1 (show $ length numLightsOn)
             let lightBrightness = sum . getLightIntStates $ input
             printResult 2 (show lightBrightness)
  where
    printResult :: Int -> String -> IO ()
    printResult = printf "result %d: %s\n"

getLightIntStates :: String -> [Int]
getLightIntStates = map getIntLightState . allPointInstructions . getRangeInstructions

getLightOnOffStates :: String -> [LightState]
getLightOnOffStates = map getOnOffLightState . allPointInstructions . getRangeInstructions

getRangeInstructions :: String -> [RangeInstruction]
getRangeInstructions = map getRangeInstruction . lines

getIntLightState :: [PointInstruction] -> Int
getIntLightState = foldl' updateLightIntState 0 . map changeType

updateLightIntState :: Int -> ChangeType -> Int
updateLightIntState n On     = n+1
updateLightIntState n Off    = max 0 (n-1)
updateLightIntState n Toggle = n+2

getOnOffLightState :: [PointInstruction] -> LightState
getOnOffLightState = foldl' updateLightOnOffState LightOff . map changeType

updateLightOnOffState :: LightState -> ChangeType -> LightState
updateLightOnOffState _ On = LightOn
updateLightOnOffState _ Off = LightOff
updateLightOnOffState s Toggle | s == LightOn = LightOff
                               | otherwise    = LightOn

-- Each sublist returned is the list of instructions for a particular coordinate
allPointInstructions :: [RangeInstruction] -> [[PointInstruction]]
allPointInstructions = group . sort . concatMap getPointInstructions

getPointInstructions :: RangeInstruction -> [PointInstruction]
getPointInstructions (RangeInstruction ct p1 p2) = map (PointInstruction ct) (pointsInRange p1 p2)
  where
    pointsInRange :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
    pointsInRange (x1,y1) (x2,y2) = map tuplify $ sequence [[x1..x2], [y1..y2]]

getRangeInstruction :: String -> RangeInstruction
getRangeInstruction str = let ct      = getChangeType str
                              (c1,c2) = parseCoords str
                          in RangeInstruction ct c1 c2

getChangeType :: String -> ChangeType
getChangeType str | "turn on"  `isPrefixOf` str = On
                  | "turn off" `isPrefixOf` str = Off
                  | "toggle"   `isPrefixOf` str = Toggle
                  | otherwise                   = undefined

parseCoords :: String -> ((Int,Int), (Int,Int))
parseCoords = parseCoords' . extractCoordStrings
  where
    extractCoordStrings :: String -> (String,String)
    extractCoordStrings = tuplify . dropMid . splitOn " " . dropWhile (not . isDigit)

    parseCoords' :: (String,String) -> ((Int,Int), (Int,Int))
    parseCoords' = mapTuple parseCoord
      where
        mapTuple :: (a -> b) -> (a,a) -> (b,b)
        mapTuple f (a,b) = (f a, f b)
        -- mapTuple f = uncurry ((,) `on` f)
        -- mapTuple f = (,) <$> f . fst <*> f . snd
        -- mapTuple = join bimap -- Data.Bifunctor
        -- mapTuple = join (***) -- Control.Arrow

    parseCoord :: String -> (Int,Int)
    parseCoord = tuplify . map read . splitOn ","
        
    dropMid :: [a] -> [a]
    dropMid [x,_,z] = [x,z]
    dropMid _       = undefined

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)
tuplify _     = undefined


-------------------------------------------------------------------------------
-- TESTS ---------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ getChangeType "turn on 660,55 through 986,197"   ~?= On
  , getChangeType "turn off 341,304 through 638,850" ~?= Off
  , getChangeType "toggle 322,558 through 977,958"   ~?= Toggle

  , parseCoords "456,345 through 123,987" ~?= (((456,345),(123,987))::((Int,Int),(Int,Int)))

  , getRangeInstruction "turn on 660,55 through 986,197"   ~?= RangeInstruction On     (660,55)  (986,197)
  , getRangeInstruction "turn off 341,304 through 638,850" ~?= RangeInstruction Off    (341,304) (638,850)
  , getRangeInstruction "toggle 322,558 through 977,958"   ~?= RangeInstruction Toggle (322,558) (977,958)

  , getPointInstructions (RangeInstruction On (1,1) (1,1)) ~?= [ PointInstruction On (1,1) ]
  , getPointInstructions (RangeInstruction Off (0,0) (0,1)) ~?= [ PointInstruction Off (0,0)
                                                                , PointInstruction Off (0,1) 
                                                                ]
  , getPointInstructions (RangeInstruction Toggle (0,0) (1,1)) ~?= [ PointInstruction Toggle (0,0)
                                                                   , PointInstruction Toggle (0,1) 
                                                                   , PointInstruction Toggle (1,0) 
                                                                   , PointInstruction Toggle (1,1) 
                                                                   ]

  , allPointInstructions [RangeInstruction On (0,0) (0,1)
                         , RangeInstruction Off (0,0) (1,0)] ~?= [ [ PointInstruction On (0,0), PointInstruction Off (0,0) ]
                                                                 , [ PointInstruction On (0,1) ]
                                                                 , [ PointInstruction Off (1,0) ] 
                                                                 ]

  , updateLightOnOffState LightOn Off ~?= LightOff
  , updateLightOnOffState LightOn On ~?= LightOn
  , updateLightOnOffState LightOff Off ~?= LightOff
  , updateLightOnOffState LightOff On ~?= LightOn
  , updateLightOnOffState LightOn Toggle ~?= LightOff
  , updateLightOnOffState LightOff Toggle ~?= LightOn

  , getOnOffLightState [] ~?= LightOff
  , getOnOffLightState [ PointInstruction Toggle (0,0)
                  , PointInstruction Toggle (0,0)
                  , PointInstruction Toggle (0,0) ] ~?= LightOn
  , getOnOffLightState [ PointInstruction Off (0,0)
                  , PointInstruction Off (0,0) ] ~?= LightOff
  , getOnOffLightState [ PointInstruction Off (0,0)
                  , PointInstruction On (0,0) ] ~?= LightOn

  , getIntLightState [] ~?= 0
  , getIntLightState [ PointInstruction Toggle (0,0)
                  , PointInstruction Toggle (0,0)
                  , PointInstruction Toggle (0,0) ] ~?= 6
  , getIntLightState [ PointInstruction Off (0,0)
                  , PointInstruction Off (0,0) ] ~?= 0
  , getIntLightState [ PointInstruction Off (0,0)
                  , PointInstruction On (0,0) ] ~?= 1
  , getIntLightState [ PointInstruction On (0,0)
                  , PointInstruction Off (0,0) 
                  , PointInstruction Toggle (0,0) ] ~?= 2
  ]
