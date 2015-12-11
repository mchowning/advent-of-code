{-# OPTIONS_GHC -Wall -Werror #-}

module Day9 where

import Data.List
import Data.Maybe
import Test.HUnit
import Text.Printf

results :: IO ()
results = do input <- readFile "day9_input.txt"
             printResult 1 . head . getSortedDistances $ input
             printResult 2 . last . getSortedDistances $ input
  where
    printResult :: Int -> Int -> IO ()
    printResult = printf "result %d: %d\n"

data Route = Route { start    :: String
                   , end      :: String
                   , distance :: Int
                   } deriving (Eq, Show)

type Location = String

-- travel to all possible destinations
getSortedDistances :: String -> [Int]
getSortedDistances input = let routes = parseRoutes input
                               allTrips = permutations . allLocations $ routes
                               -- TODO has to be a better way that (map fromJust . filter isJust)
                           in sort . map fromJust . filter isJust . map (getTripDistance routes) $ allTrips

getTripDistance :: [Route] -> [Location] -> Maybe Int
getTripDistance rs ls = let distances = map legDistance tripLegs
                        in if Nothing `elem` distances then Nothing else Just (sum . map fromJust $ distances)
  where
    tripLegs :: [(Location,Location)]
    tripLegs = map (\(x:y:_) -> (x,y)) . filter ((>1) . length) . tails $ ls

    legDistance :: (Location,Location) -> Maybe Int
    legDistance lsTup = let mRoute = uncurry (getRoute rs) lsTup
                        in if isNothing mRoute then Nothing else Just (distance . fromJust $ mRoute)

allLocations :: [Route] -> [Location]
allLocations = nub . foldr (\(Route s e _) acc -> s:e:acc) []

getRoute :: [Route] -> Location -> Location -> Maybe Route
getRoute rs l1 l2 = let matches = filter routeMatches rs
                    in if length matches < 1 then Nothing else Just (head matches)
  where
    routeMatches :: Route -> Bool
    routeMatches (Route r1 r2 _) = r1 == l1 && r2 == l2
                                || r1 == l2 && r2 == l1

parseRoutes :: String -> [Route]
parseRoutes = map parseRoute . lines
  where 
    parseRoute :: String -> Route
    parseRoute str = let ws = words str
                         departure = head ws
                         destination = ws !! 2
                         dist = last ws
                     in Route departure destination (read dist)
                 


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ parseRoutes "start1 to finish1 = 723\
              \\nstart2 to finish2 = 143" ~?= [ Route "start1" "finish1" 723
                                              , Route "start2" "finish2" 143]

  , allLocations [ Route "1" "2" 0
                 , Route "3" "4" 0
                 , Route "1" "5" 0 ] ~?= ["1","2","3","4","5"] -- FIXME this should be some type of contains test

  , getRoute [ Route "1" "2" 0
             , Route "3" "4" 0
             , Route "1" "5" 0 ] "3" "4" ~?= Just (Route "3" "4" 0)

  , getRoute [ Route "1" "2" 0
             , Route "3" "4" 0
             , Route "1" "5" 0 ] "1" "3" ~?= Nothing

  , getTripDistance_test
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
