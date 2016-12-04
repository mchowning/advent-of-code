{-# OPTIONS_GHC -Wall -Werror #-}

module Day9 where

import Data.List
import Data.Maybe
import Test.HUnit
import Text.Printf

import qualified Data.Set as S

results :: IO ()
results = do sortedDistances <- getSortedDistances <$> readFile "day9_input.txt"
             printResult 1 (head sortedDistances)
             printResult 2 (last sortedDistances)
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

data Route = Route { start    :: String
                   , end      :: String
                   , distance :: Int
                   } deriving (Eq, Show)

type Location = String
type TripLeg = (Location, Location)

-- distances for all possible trips
getSortedDistances :: String -> [Int]
getSortedDistances input =
  let routes = parseRoutes input
      allPossibleTrips = permutations . S.toList . allLocations $ routes
  in sort . mapMaybe (getTripDistance routes) $ allPossibleTrips

allLocations :: [Route] -> S.Set Location
allLocations = foldr addRouteLocations S.empty
  where
    addRouteLocations ::  Route -> S.Set Location -> S.Set Location
    addRouteLocations (Route l1 l2 _) = S.insert l1 . S.insert l2

-- Using sequence so total distance is Nothing if any leg distance is Nothing
getTripDistance :: [Route] -> [Location] -> Maybe Int
getTripDistance rts locs = sum <$> sequence legDistances
  where
    legDistances :: [Maybe Int]
    legDistances = map legDistance tripLegs

    tripLegs :: [TripLeg]
    -- tripLegs = map (\(x:y:_) -> (x,y)) . filter ((>1) . length) . tails $ locs
    tripLegs = mapMaybe getPair (tails locs)

    getPair :: [a] -> Maybe (a,a)
    getPair (x:y:_) = Just (x,y)
    getPair _       = Nothing

    legDistance :: TripLeg -> Maybe Int
    legDistance leg = distance <$> getRouteForLeg rts leg

getRouteForLeg :: [Route] -> TripLeg -> Maybe Route
getRouteForLeg rs (l1,l2) = listToMaybe $ filter routeMatches rs
  where
    routeMatches :: Route -> Bool
    routeMatches (Route r1 r2 _) = all (`elem` [l1,l2]) [r1,r2]

parseRoutes :: String -> [Route]
parseRoutes = map parseRoute . lines
  where 
    parseRoute :: String -> Route
    parseRoute str = let [departure,_,destination,_,dist] = words str
                     in Route departure destination (read dist)
                 


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseRoutes "start1 to finish1 = 723\
              \\nstart2 to finish2 = 143" ~?= [ Route "start1" "finish1" 723
                                              , Route "start2" "finish2" 143]

  , let locationList = allLocations [ Route "1" "2" 0
                                    , Route "3" "4" 0
                                    , Route "1" "5" 0 ]
        expectedLocations = ["1","2","3","4","5"]
    in all (`elem` expectedLocations) locationList &&
       length locationList == length expectedLocations ~? "should extract all locations from list of routes without duplicates"

  , getRouteForLeg [ Route "1" "2" 0
                   , Route "3" "4" 0
                   , Route "1" "5" 0 ] ("3","4") ~?= Just (Route "3" "4" 0)

  , getRouteForLeg [ Route "1" "2" 0 ] ("1","3") ~?= Nothing

  , getTripDistance_test
  , getTripDistance [] ["1", "2"] == Nothing ~? "missing routes should result in Nothing, not Just 0"
  , getShortestDistance_test
  ]
    where
      getTripDistance_test :: Test
      getTripDistance_test = let routes = [ Route "1" "2"     1 
                                          , Route "2" "3"   100
                                          , Route "4" "3"    10
                                          , Route "4" "5"  1000 
                                          , Route "1" "8" 10000
                                          , Route "1" "3" 20000
                                          , Route "1" "4" 30000
                                          , Route "3" "2" 40000
                                          , Route "5" "3" 50000
                                          , Route "4" "3" 60000
                                          ]
                                 locations = ["1","2","3","4","5"]
                             in getTripDistance routes locations ~?= Just 1111
      getShortestDistance_test :: Test
      getShortestDistance_test = let input = unlines [ "a to b = 2"
                                                     , "a to c = 1" -- fast
                                                     , "c to b = 1" -- fast
                                                     ]
                             in head (getSortedDistances input) ~?= 2
