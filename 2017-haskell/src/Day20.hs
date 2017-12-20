{-# LANGUAGE MultiWayIf #-}
module Day20 (part1, part2) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void (Void)
import Data.List (sortOn,sortBy,maximumBy,minimumBy)

import Test.Tasty
import Test.Tasty.HUnit

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type Acceleration = (Int, Int, Int)
type ID = Int
data Particle = Particle Int Position Velocity Acceleration deriving (Eq, Show)

part1, part2 :: IO Int
part1 = minimumEventualDistance <$> input
part2 = undefined

--299 too high
-- 119 is right?

input :: IO [Particle]
input =
  let filename = "src/input_day20.txt"
  in makeParticles . processEither . parse parseLines filename <$> readFile filename
 where
 
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  makeParticles :: [(Position, Velocity, Acceleration)] -> [Particle]
  makeParticles pvas = map (\(i,(p,v,a)) -> Particle i p v a) (zip [0..] pvas)
  
  parseLines :: Parsec Void String [(Position, Velocity, Acceleration)]
  parseLines = parseParticle `sepBy` eol

  parseParticle = do
    _ <- string "p="
    pos <- parseTuple
    _ <- string ", v="
    vel <- parseTuple
    _ <- string ", a="
    acc <- parseTuple
    return (pos, vel, acc)
    
  parseTuple = do
    x <- char '<' *> signed mempty decimal
    y <- char ',' *> signed mempty decimal
    z <- char ',' *> signed mempty decimal <* char '>'
    return (x,y,z)
  
------------------------------------------------------------------------------------------

minimumEventualDistance :: [Particle] -> Int
minimumEventualDistance = getId . minimumBy eventualDistance
 where
  getId :: Particle -> Int
  getId (Particle i _ _ _) = i

-- FIXME
eventualDistance :: Particle -> Particle -> Ordering
eventualDistance (Particle _ p1 s1 a1) (Particle _ p2 s2 a2) =
  case absComparison a1 a2 of
    GT -> GT
    LT -> LT
    EQ ->
      case absAdjustedComparison s1 a1 s2 a2 of
        GT -> GT
        LT -> LT
        EQ ->
          case absComparison s1 s2 of
            GT -> GT
            LT -> LT
            EQ ->
              case absAdjustedComparison p1 s1 p2 s2 of
                GT -> GT
                LT -> LT
                EQ -> absComparison p1 p2
      
   where
     absComparison a b = compare (absSum a) (absSum b)
     absSum (x,y,z) = abs x + abs y + abs z
     
     absAdjustedComparison v1 a1 v2 a2 = compare (absAdjustedSum v1 a1) (absAdjustedSum v2 a2)
     absAdjustedSum (x1, y1, z1) (x2, y2, z2) = getSign x1 x2 + getSign y1 y2 + getSign z1 z2
      where
       getSign a b = case compare (a * b) 0 of
         GT -> 1
         EQ -> 0
         LT -> -1

  
------------------------------------------------------------------------------------------

sampleInput = [ Particle 0 (3,0,0) (-3,0,0) (1,0,0)
              , Particle 1 (2,0,0) (-3,0,0) (-1,0,0)
              ]

tests :: IO ()
tests = defaultMain $ testGroup "Day 20"
  [ testGroup "part 1"
    [ testCase "sample input" $
        minimumEventualDistance [ Particle 0 (3,0,0) (2,0,0) (1,0,0)
                       , Particle 1 (4,0,0) (0,0,0) (-2,0,0)
                       ] @?= 0
    , testCase "sample input - equal acceleration counteracts velocity" $
        minimumEventualDistance [ Particle 0 (3,0,0) (-3,0,0) (1,0,0)
                       , Particle 1 (2,0,0) (-3,0,0) (-1,0,0)
                       ] @?= 0
    , testCase "sample input - equal acceleration" $
        minimumEventualDistance [ Particle 0 (3,0,0) (2,0,0) (1,0,0)
                       , Particle 1 (2,0,0) (-3,0,0) (-1,0,0)
                       ] @?= 0
    , testCase "sample input - equal acceleration and velocity" $
        minimumEventualDistance [ Particle 0 (3,0,0) (3,0,0) (1,0,0)
                       , Particle 1 (2,0,0) (3,0,0) (1,0,0)
                       ] @?= 1
    , testCase "real input" $ part1 >>= (@?= 119)
    ]
  ]
