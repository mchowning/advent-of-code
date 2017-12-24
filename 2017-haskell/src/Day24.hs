{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day24 (part1, part2) where

import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, parse, parseErrorPretty, sepBy,
                                       some)
import Data.Ord (comparing)
import           Text.Megaparsec.Char (char, digitChar, eol)

import           Test.Tasty
import           Test.Tasty.HUnit
import Data.List (delete,nub,maximumBy)
import Data.Maybe (mapMaybe)

data Port =
  Port { _input :: Int
       , _output :: Int
       } deriving (Show, Eq)

type Bridge = V.Vector Port

part1, part2 :: IO Int
part1 = strongestPossible <$> input
part2 = longestStrongestPossible <$> input

strongestPossible :: [Port] -> Int
strongestPossible = maximum . map bridgeStrength . allPossibleBridges

longestStrongestPossible :: [Port] -> Int
longestStrongestPossible = bridgeStrength . longestBridge . allPossibleBridges

longestBridge :: [Bridge] -> Bridge
longestBridge bs =
  maximumBy (comparing bridgeStrength) . filter lengthEqualsLongest $ bs
 where
  lengthEqualsLongest b = length b == length (maximumBy (comparing V.length) bs)

allPossibleBridges :: [Port] -> [Bridge]
allPossibleBridges = allPossibleBridges' . (V.empty, 0,)
 where
 
  allPossibleBridges' :: (Bridge, Int, [Port]) -> [Bridge]
  allPossibleBridges' (bridge, _, []) = [bridge]
  allPossibleBridges' (bridge, openConnection, ports) =
    let allAdditions = matchWithRemaining openConnection ports
    in if null allAdditions
         then []
         else let allNewBridgesWithRemainingPorts = map (\(p,i,remPs) -> (V.snoc bridge p, i, remPs)) allAdditions
                                                 -- map (\(p, ps) -> (V.snoc bridge, ps)) allAdditions
                  allNewBridges = map (\(newB,_,_) -> newB) allNewBridgesWithRemainingPorts
              in allNewBridges ++ concatMap allPossibleBridges' allNewBridgesWithRemainingPorts
            
  matchWithRemaining :: Int -> [Port] -> [(Port, Int, [Port])]
  matchWithRemaining n ps =
    map withRestOfList . nub . mapMaybe portMatchesWithOpenConnection $ ps
   where
   
    portMatchesWithOpenConnection :: Port -> Maybe (Port, Int)
    portMatchesWithOpenConnection p@(Port begin end)
      | begin == n = Just (p, end)
      | end == n = Just (p, begin)
      | otherwise = Nothing
    withRestOfList (p,i) = (p, i, delete p ps)

bridgeStrength :: Bridge -> Int
bridgeStrength = sum . V.map portStrength
 where
  portStrength (Port p1 p2) = p1 + p2

input :: IO [Port]
input =
  let filename = "src/input_day24.txt"
  in processEither . parse parseFile filename <$> readFile filename
 where
  parseFile = line `sepBy` eol

  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  line :: Parsec Void String Port
  line = do
    p1 <- read <$> some digitChar
    _ <- char '/'
    p2 <- read <$> some digitChar
    return (Port p1 p2)

------------------------------------------------------------------------------------------

sampleInput :: [Port]
sampleInput =
  [ Port 0 2
  , Port 2 2
  , Port 2 3
  , Port 3 4
  , Port 3 5
  , Port 0 1
  , Port 10 1
  , Port 9 10
  ]

tests = defaultMain $ testGroup "Day 24"
  [ testCase "bridge strength" $
      bridgeStrength (V.fromList [ Port 0 1
                                 , Port 10 1
                                 , Port 9 10 ]) @?= 31
  , testGroup "part 1"
    [ testCase "sample input" $ strongestPossible sampleInput @?= 31
--    , testCase "real input" $ part1 >>= (@?= 1695) -- bit slow
    ]
  , testGroup "part 2"
    [ testCase "sampleInput" $ longestStrongestPossible sampleInput @?= 19
--    , testCase "real input" $ part2 >>= (@?= 1673) -- bit slow
    ]
  ]
