{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Day12 where

import Util

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Set as S

import Linear.V3
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

{-
- for each time step:
  - first update each moon's velocity by applying gravity (starts at 0)
    - To apply gravity, consider every pair of moons. On each axis (x, y, and z),
      the velocity of each moon changes by exactly +1 or -1 to pull the moons
      together. If the positions on a given axis are the same, the velocity on
      that axis does not change for that pair of moons.
  - then update each moon's position
  - total energy of the system is the moons' potential energy multiplied by its kinetic energy
    - potential energy is the sum of the absolute values of its x, y, and z coordinates
    - kinetic energy is the sum of teh absolute values of its velocity coordinates
-}

newtype Coordinate = Coordinate Int
                     deriving (Eq, Num, Ord, Show)

newtype Velocity = Velocity Int
                   deriving (Eq, Num, Ord, Show)

plus :: V3 Coordinate -> V3 Velocity -> V3 Coordinate
plus = liftA2 plus'
  where
    plus' (Coordinate c) (Velocity v) = Coordinate (c + v)

data Moon = Moon { coordinates :: V3 Coordinate
                 , velocity :: V3 Velocity
                 }
                 deriving (Eq, Ord)

instance Show Moon where
  show (Moon (V3 (Coordinate c1) (Coordinate c2) (Coordinate c3))
             (V3 (Velocity v1) (Velocity v2) (Velocity v3))) =
    "Moon { Coordinates = " <> show (c1,c2,c3) <> ", Velocity = " <> show (v1,v2,v3) <> " }"

part1 :: Int
part1 =
  let initial = initialize (parseText input)
      after1000 = stateAfter 1000 initial
      energies = S.map totalEnergy after1000
  in sum energies

initialize :: S.Set (V3 Coordinate) -> S.Set Moon
initialize = S.map (flip Moon (Velocity <$> V3 0 0 0))

stateAfter :: Int -> S.Set Moon -> S.Set Moon
stateAfter n = (!! n) . iterate timeStep

timeStep :: S.Set Moon -> S.Set Moon
timeStep = S.map updateCoordinates . updateVelocities

totalEnergy :: Moon -> Int
totalEnergy Moon {..} =
  let (Coordinate cSum) = sum (abs <$> coordinates)
      (Velocity vSum) = sum (abs <$> velocity)
  in cSum * vSum

updateCoordinates :: Moon -> Moon
updateCoordinates Moon {..} = Moon { coordinates = coordinates `plus` velocity, velocity }

updateVelocities :: S.Set Moon -> S.Set Moon
--updateVelocities ms = S.map (\m -> updateVelocity (S.delete m ms) m) ms
updateVelocities ms = S.map (updateVelocity =<< flip S.delete ms) ms

updateVelocity :: S.Set Moon -> Moon -> Moon
updateVelocity ms m = S.foldl' updateVelocity' m ms

updateVelocity' :: Moon -> Moon -> Moon
updateVelocity' (Moon coordinates velocity) (Moon otherCoordinates _) =
  let change = velocityChange coordinates otherCoordinates
  in Moon coordinates (velocity + change)

velocityChange :: V3 Coordinate -> V3 Coordinate -> V3 Velocity
velocityChange coordinates otherCoordinates =
  let diff = otherCoordinates - coordinates
  in (\(Coordinate c) -> Velocity c) <$> signum diff

-------------------------------------------------------------------------------------

-- 484244804958744
part2 :: Int
part2 =
  let initial = initialize (parseText input)
  in stepsToFirstDuplicate timeStep initial S.empty

test :: Int -> Int
test n =
  let initial = convert n . initialize . parseText $ input
      steps = stepsToFirstDuplicate timeStep1 initial S.empty
  in steps

convert :: Int -> S.Set Moon -> S.Set (Coordinate, Velocity)
convert n = S.map (convert' n) 

convert' :: Int -> Moon -> (Coordinate, Velocity)
convert' n (Moon (V3 c1 c2 c3) (V3 v1 v2 v3)) = case n of
  1 -> (c1, v1)
  2 -> (c2, v2)
  3 -> (c3, v3)
  n -> error ("enexpected convert integer: " <> show n)



stepsToFirstDuplicate :: Ord a => (S.Set a -> S.Set a) -> S.Set a -> S.Set (S.Set a) -> Int
stepsToFirstDuplicate stepper state seen =
  if S.member state seen
    then 0
    else let newState = stepper state
         in 1 + stepsToFirstDuplicate stepper newState (S.insert state seen)


timeStep1 :: S.Set (Coordinate, Velocity) -> S.Set (Coordinate, Velocity)
timeStep1 = S.map updateCoordinates1 . updateVelocities1

updateCoordinates1 :: (Coordinate, Velocity) -> (Coordinate, Velocity)
updateCoordinates1 (Coordinate c, Velocity v) = (Coordinate (c + v), Velocity v)

updateVelocities1 :: S.Set (Coordinate, Velocity) -> S.Set (Coordinate, Velocity)
updateVelocities1 s = S.map (updateVelocity1 =<< flip S.delete s) s

updateVelocity1 :: S.Set (Coordinate, Velocity) -> (Coordinate, Velocity) -> (Coordinate, Velocity)
updateVelocity1 s cv = S.foldl' updateVelocity1' cv s

updateVelocity1' :: (Coordinate, Velocity) -> (Coordinate, Velocity) -> (Coordinate, Velocity)
updateVelocity1' (Coordinate c, Velocity v) (Coordinate otherC, Velocity otherV) =
  let change = signum (otherC - c)
  in (Coordinate c, Velocity (v + change))










-------------------------------------------------------------------------------------

parseText :: Text -> S.Set (V3 Coordinate)
parseText = S.fromList . parse' (parseLine `sepBy1` eol) "day12"
  where

    parseLine :: Parser (V3 Coordinate)
    parseLine = do
      void (string "<x=")
      x <- signedInt
      void (string ", y=")
      y <- signedInt
      void (string ", z=")
      z <- signedInt
      void (string ">")
      return (Coordinate <$> V3 x y z)

    signedInt = signed (return()) decimal

input :: Text
input = "<x=-3, y=10, z=-1>\n\
        \<x=-12, y=-10, z=-5>\n\
        \<x=-9, y=0, z=10>\n\
        \<x=7, y=-5, z=-3>"
