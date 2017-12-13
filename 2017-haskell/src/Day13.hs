{-# LANGUAGE MultiWayIf #-}
module Day13 where

import           Control.Monad        (mfilter)
import           Data.List            (map, sum)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust, isJust, mapMaybe)
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

data Firewall = Firewall
  { _packetLayer :: Integer
  , _layers      :: M.Map Integer Layer
  } deriving (Eq, Show)

part1 :: IO Integer
part1 = part1Algorithm <$> input

part2 :: IO Integer
part2 = undefined

part1Algorithm :: M.Map Integer Layer -> Integer
part1Algorithm =
  sum . map calculateSeverity . filter isPacketCaught . buildStateList

isPacketCaught:: Firewall -> Bool
isPacketCaught (Firewall pl m) =
  let packetLayer = M.lookup pl m
  in isJust packetLayer && (0 == _scannerPosition (fromJust packetLayer))

packetCaughtAtLayer:: Firewall -> Maybe Layer
packetCaughtAtLayer (Firewall pl m) =
  let packetLayer = M.lookup pl m
  in mfilter ((0 ==) . _scannerPosition) packetLayer

buildStateList :: M.Map Integer Layer -> [Firewall]
buildStateList m = recurseMaybe (getNextLayer m) (Firewall 0 m)

recurseMaybe :: (a -> Maybe a) -> a -> [a]
recurseMaybe f a =
  let result = f a
  in if isJust result
      then a : recurseMaybe f (fromJust result)
      else [a]

getNextLayer :: M.Map Integer Layer -> Firewall -> Maybe Firewall
getNextLayer m f@(Firewall packetPos _) =
  let lastLayer = last (M.keys m)
  in if packetPos > lastLayer
       then Nothing
       else Just (updateFirewall f)

calculateSeverity :: Firewall -> Integer
calculateSeverity f =
  let index = _packetLayer f
      range = _range (_layers f M.! index)
  in index * range

updateLayer :: Layer -> Layer
updateLayer (Layer range pos mov) =
  let newPos = if mov == Ascending then pos+1 else pos-1
      newMov = if
        | newPos == 0     -> Ascending
        | newPos == range - 1 -> Descending
        | otherwise       -> mov
  in Layer range newPos newMov

updateFirewall :: Firewall -> Firewall
updateFirewall (Firewall packetLayer m) = Firewall (packetLayer +1) (M.map updateLayer m)

input :: IO (M.Map Integer Layer)
input =
  let filename = "src/input_day13.txt"
  in processEither . parse file filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  file :: Parsec Void String (M.Map Integer Layer)
  file = M.fromList <$> line `sepBy` newline
  
  line :: Parsec Void String (Integer, Layer)
  line = do
    depth <- read <$> some digitChar
    _     <- string ": "
    range <- read <$> some digitChar
    return (depth, Layer range 0 Ascending)

-------------------------------------------------------------------------------

sampleInput = M.fromList
  [ (0, Layer 3 0 Ascending)
  , (1, Layer 2 0 Ascending)
  , (4, Layer 4 0 Ascending)
  , (6, Layer 4 0 Ascending)
  ]
tests :: IO ()
tests = defaultMain $ testGroup "Day 13"
 [ updateLayerTests
 , part1Tests ]
 where
  updateLayerTests =
    testGroup
    "updating scanner position"
    [ testCase "0 -> 1" $ updateLayer (Layer 3 0 Ascending) @?= Layer 3 1 Ascending
    , testCase "1 -> 2" $ updateLayer (Layer 3 1 Ascending) @?= Layer 3 2 Descending
    , testCase "2 -> 1" $ updateLayer (Layer 3 2 Descending) @?= Layer 3 1 Descending
    , testCase "1 -> 0" $ updateLayer (Layer 3 1 Descending) @?= Layer 3 0 Ascending
    ]
  part1Tests =
    testGroup
    "Part 1"
    [ testCase "sample input" $ part1Algorithm sampleInput @?= 24 ]
