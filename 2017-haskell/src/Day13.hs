{-# LANGUAGE MultiWayIf #-}
module Day13 where

import           Data.List            (map, sum)
import qualified Data.Map.Strict      as M
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, parse, parseErrorPretty, sepBy,
                                       some)
import           Text.Megaparsec.Char (digitChar, newline, string)

import           Test.Tasty
import           Test.Tasty.HUnit

data ScannerMovement
  = Ascending
  | Descending
  deriving (Eq, Show)

data Layer = Layer
  { _range           :: Integer
  , _scannerPosition :: Integer
  , _scannerMovement :: ScannerMovement
  } deriving (Eq, Show)

type PacketPosition = Integer
type FirewallState = M.Map Integer Layer
type PacketProgression = [(PacketPosition, FirewallState)]

part1 :: IO Integer
part1 = part1Algorithm <$> input

part2 :: IO Int
part2 = part2Algorithm <$> input

part1Algorithm :: FirewallState -> Integer
part1Algorithm m =
  sum . map (calculateSeverity m) . layersCatchingPacketWithImmediateStart $ m

part2Algorithm :: FirewallState -> Int
part2Algorithm m = head . filter (packetIsCaughtAtStartTime m) $ [0..]

layersCatchingPacketWithImmediateStart :: FirewallState  -> [PacketPosition]
layersCatchingPacketWithImmediateStart = map fst . filter catchPacket . M.toList
 where
  catchPacket (pos, Layer range _ _) = layerScannerAt0 pos range

packetIsCaughtAtStartTime :: FirewallState -> Int -> Bool
packetIsCaughtAtStartTime fs n = not . any catchPacket $ M.toList fs
 where
  catchPacket (pos, Layer range _ _) = layerScannerAt0 (pos + fromIntegral n) range

calculateSeverity :: FirewallState -> PacketPosition -> Integer
calculateSeverity fws index =
  let range = _range (fws M.! index)
  in index * range

layerScannerAt0 :: Integer -> Integer -> Bool
layerScannerAt0 _ 1 = True
layerScannerAt0 0 _ = True
layerScannerAt0 time range =
  let at0Every = 2 * (range - 1)
  in time `rem` at0Every == 0

input :: IO FirewallState
input =
  let filename = "src/input_day13.txt"
  in processEither . parse file filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  file :: Parsec Void String FirewallState
  file = M.fromList <$> line `sepBy` newline

  line :: Parsec Void String (Integer, Layer)
  line = do
    depth <- read <$> some digitChar
    _     <- string ": "
    range <- read <$> some digitChar
    return (depth, Layer range 0 Ascending)

-------------------------------------------------------------------------------

sampleInput :: FirewallState
sampleInput = M.fromList
  [ (0, Layer 3 0 Ascending)
  , (1, Layer 2 0 Ascending)
  , (4, Layer 4 0 Ascending)
  , (6, Layer 4 0 Ascending)
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
    [ testCase "sample input" $ part1Algorithm sampleInput @?= 24
    , testCase "real input" $ part1 >>= (@?= 1316)
    ]
  part2Tests =
    testGroup
    "Part 2"
    [ testCase "sample input" $ part2Algorithm sampleInput @?= 10
--    , testCase "real input" $ part2 >>= (@?= 3840052) -- slow (14s)
    ]
