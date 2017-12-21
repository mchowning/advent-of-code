{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day13 (part1, part2) where

import           Data.List            (sum)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, parse, parseErrorPretty, sepBy,
                                       some)
import           Text.Megaparsec.Char (digitChar, newline, string)

import           Test.Tasty
import           Test.Tasty.HUnit

data Layer = Layer { _position :: Integer
                   , _range    :: Integer
                   } deriving (Eq, Show, Ord) -- a Layer should have a position and a range

---------

-- time                 148.5 ms   (146.1 ms .. 153.6 ms)
--                      0.999 R²   (0.996 R² .. 1.000 R²)
-- mean                 149.1 ms   (147.2 ms .. 150.9 ms)
-- std dev              2.484 ms   (1.558 ms .. 3.581 ms)
-- variance introduced by outliers: 12% (moderately inflated)

part1 :: IO Integer
part1 = caughtSeverityFrom0 <$> input

caughtSeverityFrom0 :: S.Set Layer -> Integer
caughtSeverityFrom0 = sum . S.map calculateSeverity . layersCatchingPacketWithImmediateStart

layersCatchingPacketWithImmediateStart :: S.Set Layer  -> S.Set Layer
layersCatchingPacketWithImmediateStart = S.filter (catchPacket 0)

calculateSeverity :: Layer -> Integer
calculateSeverity (Layer position range) = position * range

----------

-- time                 883.5 ms   (871.5 ms .. 893.4 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 883.1 ms   (880.8 ms .. 884.7 ms)
-- std dev              2.342 ms   (0.0 s .. 2.702 ms)
-- variance introduced by outliers: 19% (moderately inflated)

part2 :: IO Int
part2 = firstChanceToNotBeCaught <$> input

firstChanceToNotBeCaught :: S.Set Layer -> Int
firstChanceToNotBeCaught s = head . filter (packetIsCaughtAtStartTime s) $ [0..]

packetIsCaughtAtStartTime :: S.Set Layer -> Int -> Bool
packetIsCaughtAtStartTime s n = not . any (catchPacket $ toInteger n) $ S.elems s

layerScannerAt0 :: Integer -> Integer -> Bool
layerScannerAt0 _ 1 = True
layerScannerAt0 time range =
  let at0Every = 2 * (range - 1)
  in time `rem` at0Every == 0
  
----------


catchPacket :: Integer -> Layer -> Bool
catchPacket startTime (Layer position range) = layerScannerAt0 (position + startTime) range

input :: IO (S.Set Layer)
input =
  let filename = "src/input_day13.txt"
  in processEither . parse file filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  file :: Parsec Void String (S.Set Layer)
  file = S.fromList <$> line `sepBy` newline

  line :: Parsec Void String Layer
  line = do
    depth <- read <$> some digitChar
    _     <- string ": "
    range <- read <$> some digitChar
    return (Layer depth range)

-------------------------------------------------------------------------------

sampleInput :: S.Set Layer
sampleInput = S.fromList
  [ Layer 0 3
  , Layer 1 2
  , Layer 4 4
  , Layer 6 4
  ]
tests :: IO ()
tests = defaultMain $ testGroup "Day 13"
 [ part1Tests
 , part2Tests
 , layerScannerAt0Tests
 ]
 where
  layerScannerAt0Tests =
    testGroup
    "layerScannerAt0"
    [ testCase "time 0 range 1" $ layerScannerAt0 0 1 @?= True
    , testCase "time 1 range 1" $ layerScannerAt0 1 1 @?= True
    , testCase "time 2 range 1" $ layerScannerAt0 2 1 @?= True

    , testCase "time 0 range 2" $ layerScannerAt0 0 2 @?= True
    , testCase "time 1 range 2" $ layerScannerAt0 1 2 @?= False
    , testCase "time 2 range 2" $ layerScannerAt0 2 2 @?= True

    , testCase "time 0 range 3" $ layerScannerAt0 0 3 @?= True
    , testCase "time 1 range 3" $ layerScannerAt0 1 3 @?= False
    , testCase "time 2 range 3" $ layerScannerAt0 2 3 @?= False
    , testCase "time 3 range 3" $ layerScannerAt0 3 3 @?= False
    , testCase "time 4 range 3" $ layerScannerAt0 4 3 @?= True

    , testCase "time 0 range 4" $ layerScannerAt0 0 4 @?= True
    , testCase "time 1 range 4" $ layerScannerAt0 1 4 @?= False
    , testCase "time 2 range 4" $ layerScannerAt0 2 4 @?= False
    , testCase "time 3 range 4" $ layerScannerAt0 3 4 @?= False
    , testCase "time 4 range 4" $ layerScannerAt0 4 4 @?= False
    , testCase "time 4 range 4" $ layerScannerAt0 5 4 @?= False
    , testCase "time 4 range 4" $ layerScannerAt0 6 4 @?= True
    ]
  part1Tests =
    testGroup
    "Part 1"
    [ testCase "sample input" $ caughtSeverityFrom0 sampleInput @?= 24
    , testCase "real input" $ part1 >>= (@?= 1316)
    ]
  part2Tests =
    testGroup
    "Part 2"
    [ testCase "sample input" $ firstChanceToNotBeCaught sampleInput @?= 10
--    , testCase "real input" $ part2 >>= (@?= 3840052) -- slow (14s)
    ]
