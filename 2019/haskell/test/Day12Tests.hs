{-# LANGUAGE OverloadedStrings #-}
module Day12Tests where

import Day12

import qualified Data.Set as S
import Data.Text (Text)
import Linear.V3

import Test.Tasty
import Test.Tasty.HUnit

example1 :: Text
example1 = "<x=-1, y=0, z=2>\n\
           \<x=2, y=-10, z=-7>\n\
           \<x=4, y=-8, z=8>\n\
           \<x=3, y=5, z=-1>"

example1After10 = S.fromList [ mkMoon (2,1,-3) (-3,-2,1)
                             , mkMoon (1,-8,0) (-1,1,3)
                             , mkMoon (3,-6,1) (3,2,-3)
                             , mkMoon (2,0,4) (1,-1,-1) ]

example2 :: Text
example2 = "<x=-8, y=-10, z=0>\n\
           \<x=5, y=5, z=10>\n\
           \<x=2, y=-7, z=3>\n\
           \<x=9, y=-8, z=-3>"

example2After100 = S.fromList [ mkMoon (8,-12,-9) (-7,3,0)
                              , mkMoon (13,16,-3) (3,-11,-5)
                              , mkMoon (-29,-11,-1) (-3,7,4)
                              , mkMoon (16,-13,23) (7,1,1) ]

mkMoon :: (Int, Int, Int) -> (Int, Int, Int) -> Moon
mkMoon (c1,c2,c3) (v1,v2,v3) = Moon (Coordinate <$> V3 c1 c2 c3) (Velocity <$> V3 v1 v2 v3)

moon0 = mkMoon (0,0,0) (0,0,0)

day12Tests = testGroup "day 12"
  [ testGroup "part 1"
    [ testCase "actual input" $ part1 @?= 10944
    , testGroup "example 1"
      [ testCase "state" $ (stateAfter 10 . initialize . parseText $ example1) @?= example1After10
      , testCase "energy" $ (sum . S.map totalEnergy . stateAfter 10 . initialize . parseText $ example1) @?= 179
      ]
    , testGroup "example 2"
      [ testCase "state" $ (stateAfter 100 . initialize . parseText $ example2) @?= example2After100
      , testCase "energy" $ (sum . S.map totalEnergy . stateAfter 100 . initialize . parseText $ example2) @?= 1940
      ]
    , testGroup "update moon velocity"
      [ testCase "moon can't change itself" $
          updateVelocity' moon0 moon0 @?= moon0
      , testCase "increases velocity" $
          let updated = updateVelocity' moon0 (mkMoon (1,10,100) (0,0,0))
          in velocity updated @?= V3 1 1 1
      , testCase "decreases velocity" $
          let updated = updateVelocity' moon0 (mkMoon (-1,-10,-100) (0,0,0))
          in velocity updated @?= V3 (-1) (-1) (-1)
      , testCase "applies change to current velocity" $
          let updated = updateVelocity' (mkMoon (0,0,0) (10,10,10)) (mkMoon (1,1,1) (0,0,0))
          in velocity updated @?= V3 11 11 11
      , testCase "updates both moons" $
          let moons = updateVelocities (S.fromList [moon0, mkMoon (1,10,100) (10,10,10)])
          in S.map velocity moons @?= S.fromList [V3 1 1 1, V3 9 9 9]
      , testCase "does not change coordinates" $
          let c1 = 12
              c2 = 234
              c3 = 26789
              updated = updateVelocity' (mkMoon (c1,c2,c3) (0,0,0)) (mkMoon (100000,-100000,0) (0,0,0))
          in coordinates updated @?= Coordinate <$> V3 c1 c2 c3
      ]
    ]
  ]
