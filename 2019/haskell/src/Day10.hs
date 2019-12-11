module Day10 where

import           Data.List       (foldl', sortOn, tails, transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

type Coordinate = (Int, Int)

readInput :: IO [String]
readInput = lines <$> readFile "../inputs/day10.txt"

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [String] -> Int
part1' xs =
  let numDetectableMap = fmap length . getDetectableMap . parseCoords $ xs
  in maximum (M.elems numDetectableMap)

parseCoords :: [String] -> S.Set Coordinate
parseCoords xs =
  let withX = zip [0..] <$> xs
      withXY = concat $ zipWith (\y (x,v) -> ((x,y), v)) [0..] <$> transpose withX
      filtered = filter (\(_,v) -> v == '#') withXY
  in S.fromList (fst <$> filtered)

isBlocked :: Coordinate -> Coordinate -> S.Set Coordinate -> Bool
isBlocked (x1,y1) (x2, y2) s =
  let xDiff =  x2 - x1
      yDiff = y2 - y1
      changeFactor = gcd xDiff yDiff
      xChange = xDiff `div` changeFactor
      yChange = yDiff `div` changeFactor
      update (x,y) = (x + xChange, y + yChange)
      interceptPoints = case takeWhile (/= (x2, y2)) . iterate update $ (x1,y1) of
                          [] -> error "empty list error: original point should have been included (wassame coordinate was passed to both paramters?)"
                          xs -> tail xs

  in any (`S.member` s) interceptPoints

getDetectableMap :: S.Set Coordinate -> M.Map Coordinate [Coordinate]
getDetectableMap s =
  let tups = pairs (S.toList s)
  in foldl' updateIfNotBlocked M.empty tups
    where
      updateIfNotBlocked :: M.Map Coordinate [Coordinate] -> (Coordinate, Coordinate) -> M.Map Coordinate [Coordinate]
      updateIfNotBlocked m (c1, c2) =
        if isBlocked c1 c2 s
          then m
          else M.insertWith (++) c2 [c1]
                 (M.insertWith (++) c1 [c2] m)

pairs :: Ord a => [a] -> [(a, a)]
pairs s = [(x,y) | (x:xt) <- tails s, y <- xt]

divisible :: Int -> Int -> Bool
divisible a b = 0 == max a b `mod` min a b

-----------------------------------------------------------------------------

part2 :: IO Int
part2 = part2' <$> readInput

part2' :: [String] -> Int
part2' xs =
  let allCoords = parseCoords xs
      stationLocation = monitoringStationLocation (getDetectableMap allCoords)
      destroyedCoordsInOrder = destroy stationLocation allCoords
      (x,y) = destroyedCoordsInOrder !! 199
  in (x * 100) + y

destroy :: Coordinate -> S.Set Coordinate -> [Coordinate]
destroy center coords
  | S.size coords == 0 = []
  | otherwise =
      let (detected, undetected) = getDetected center coords
      in destructionOrder center detected ++ destroy center undetected

getDetected :: Coordinate -> S.Set Coordinate -> (S.Set Coordinate, S.Set Coordinate)
getDetected center coords =
  let detected = S.fromList (getDetectableMap coords M.! center)
      undetected = coords S.\\ detected
  in (detected, undetected)

destructionOrder :: Coordinate -> S.Set Coordinate -> [Coordinate]
destructionOrder c = sortOn (toRadians c) . S.toList

toRadians :: RealFloat a => Coordinate -> Coordinate -> a
toRadians (x1,y1) (x2,y2) =
  let rawRadians = atan2 (fromIntegral (x2 - x1)) (fromIntegral (y1 - y2)) --ys are reversed since they're descending
  in if rawRadians >= 0
       then rawRadians
       else rawRadians + (2 * pi)

monitoringStationLocation :: M.Map Coordinate [Coordinate] -> Coordinate
monitoringStationLocation m =
  let maxDetectable = maximum (length <$> M.elems m)
  in head . M.keys . M.filter ((== maxDetectable) . length) $ m
