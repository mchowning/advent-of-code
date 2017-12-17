{-# LANGUAGE ViewPatterns #-}
module Day16 (part1, part2) where

import           Data.Void            (Void)

import           Control.Applicative  (some, (<|>))
import           Data.Bits
import           Data.Char            (chr, ord)
import           Data.List            (sortBy,elemIndex)
import           Data.Maybe           (fromJust)
import           Data.Ord             (comparing)
import qualified Data.Vector as V
import           Data.Word            (Word16)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec      (Parsec, parse, parseErrorPretty, sepBy)
import           Text.Megaparsec.Char (char, digitChar, letterChar)
import qualified Data.Map.Strict as M

import           Data.List            (foldl')
import           Debug.Trace

data Move = Spin Int
          | Exchange Int Int
          | Swap Char Char
          deriving (Eq, Show)


--updateElems :: V.Vector Char -> V.Vector Char
--updateElems cs = V.fromList $ map (cs V.!) [5,6,12,14,1,4,0,8,9,7,3,15,10,2,11,13]

-----------------------------------------------------------------------------------

input :: IO [Move]
input =
  let filename = "src/input_day16.txt"
  in processEither . parse file filename <$> readFile filename
 where
  processEither (Left  e ) = error (parseErrorPretty e)
  processEither (Right rs) = rs

  file :: Parsec Void String [Move]
  file = (parseSpin <|> parseExchange <|> parseSwap) `sepBy` char ','
   where
    parseSpin, parseExchange, parseSwap :: Parsec Void String Move
    parseSpin = do
      _ <- char 's'
      n <- read <$> some digitChar
      return (Spin n)

    parseExchange = do
      _ <- char 'x'
      n1 <- read <$> some digitChar
      _ <- char '/'
      n2 <- read <$> some digitChar
      return (Exchange n1 n2)

    parseSwap = do
      _ <- char 'p'
      p1 <- letterChar
      _ <- char '/'
      p2 <- letterChar
      return (Swap p1 p2)

-----------------------------------------------------------------------------------

-- 500
-- full -> 7.2
-- no spin 6.11
-- no exchange 5.15

-- 1000 rounds
-- naive list: 6
-- vector: 1.65

-- not hcndoplfgeiamjkb

part1, part2 :: IO String
part1 = V.toList . doDance (V.fromList ['a'..'p']) <$> input
--part1 = return . V.toList . updateElems . V.fromList $ ['a'..'p']
--part1 = map word16ToChar . V.toList . doDance' <$> input
--part1 = vectorToString . doDance' getBitList <$> input
part2 = V.toList . doMultipleDances 1000000000 (V.fromList ['a'..'p']) <$> input
--part2 = vectorToString . doMultipleDances' 500 (stringToVector ['a'..'p']) <$> input
--part2 = V.toList . doMultipleDances 1000000000 (V.fromList ['a'..'p']) <$> input

doDance :: V.Vector Char -> [Move] -> V.Vector Char
doDance = foldl' executeMove

doDanceWithCheck :: (M.Map (V.Vector Char) (V.Vector Char), V.Vector Char) -> [Move] -> (M.Map (V.Vector Char) (V.Vector Char), V.Vector Char)
doDanceWithCheck (savedMap, cs) ms =
  if M.member cs savedMap
    then (savedMap, savedMap M.! cs)
    else
      let newResult = doDance cs ms
          newMap = M.insert cs newResult savedMap
      in (newMap, newResult)
      
doMultipleDances :: Int -> V.Vector Char -> [Move] -> V.Vector Char
doMultipleDances n start ms = snd (iterate (`doDanceWithCheck` ms) (M.empty, start) !! n)

--doMultipleDances :: Int -> V.Vector Char -> [Move] -> V.Vector Char
--doMultipleDances n start ms = iterate (`doDance` ms) start !! n

executeMove :: V.Vector Char -> Move -> V.Vector Char
executeMove s (Spin n)         = executeSpin s n
--executeMove s (Spin n)         = s
executeMove s (Exchange n1 n2) = executeExchange s n1 n2
--executeMove s (Exchange n1 n2) = s
executeMove s (Swap c1 c2)     = executeSwap s c1 c2
--executeMove s (Swap c1 c2)     = s

executeSpin :: V.Vector Char -> Int -> V.Vector Char
executeSpin ls n =
  let opp = V.length ls - n
  in V.drop opp ls V.++ V.take opp ls

executeExchange :: V.Vector a -> Int -> Int -> V.Vector a
executeExchange ls i1 i2 =
  let e1 = ls V.! i1
      e2 = ls V.! i2
  in ls V.// [(i1,e2), (i2,e1)]

executeSwap :: Eq a => V.Vector a -> a -> a -> V.Vector a
executeSwap ls c1 c2 =
  let i1 = fromJust (V.elemIndex c1 ls)
      i2 = fromJust (V.elemIndex c2 ls)
  in executeExchange ls i1 i2

-----------------------------------------------------------------------------------

getBitList :: V.Vector Word16
getBitList = V.fromList (map bit [0..15])

--processResult :: V.Vector Word16 -> String
--processResult ws = foldl' (\acc p -> chr (getIndexForPosition ws p + ord 'a') : acc) [] [0 .. 15]

getCharForPosition :: V.Vector Word16 -> Int -> Char
getCharForPosition ws = chr . (+ ord 'a') . getIndexForPosition ws

getIndexForPosition :: V.Vector Word16 -> Int -> Int
getIndexForPosition ws i = fst . V.head . V.filter (\(_,w) -> getBitIndex w == i) . V.indexed $ ws

doMultipleDances' :: Int -> V.Vector Word16 -> [Move] -> V.Vector Word16
doMultipleDances' n start ms = iterate (`doDance'` ms) start !! n

doDance' :: V.Vector Word16 -> [Move] -> V.Vector Word16
doDance' = foldl' executeMove'

word16ToChar :: Word16 -> Char
word16ToChar w = chr (getBitIndex w + ord 'a')

getBitIndex :: Word16 -> Int
getBitIndex w = head . filter (testBit w) $ [0..15]

executeMove' :: V.Vector Word16 -> Move -> V.Vector Word16
--executeMove' s move | trace ("state: " ++ show (V.map word16ToChar s) ++ " with move: " ++ show move) False = undefined
executeMove' s (Spin n)         = executeSpin' s n
executeMove' s (Exchange n1 n2) = executeExchange' s n1 n2
--executeMove' s (Swap c1 c2)     = executeSwap' s (getWord16 c1) (getWord16 c2)
executeMove' s (Swap c1 c2)     = executeSwap' s c1 c2

--getWord16 :: Char -> Word16
----getWord16  c = snd . V.head . V.filter ((== c) . fst) $ V.zip (V.fromList ['a'..'p']) getBitList
--getWord16 c = bit (ord c - ord 'a')


-- remember, the Word 16 represents the position of the letter for that index
executeSpin' :: V.Vector Word16 -> Int -> V.Vector Word16
--executeSpin' ls n = V.map (`rotateR` n) ls
executeSpin' ls n = V.map (`rotateL` n) ls

executeExchange' :: V.Vector Word16 -> Int -> Int -> V.Vector Word16
executeExchange' ws i1 i2 =
  let char1Pos = getIndexOfChar . findCharForIndex ws $ i1
      char2Pos = getIndexOfChar . findCharForIndex ws $ i2
      new1 = ws V.! char2Pos
      new2 = ws V.! char1Pos
  in ws V.// [(char1Pos,new1), (char2Pos,new2)]
--  let e1 = ws V.! i1
--      e2 = ws V.! i2
--      combined = e1 .|. e2
--      new1 = xor e1 combined
--      new2 = xor e2 combined
--  in ws V.// [(i1,new1), (i2,new2)]

executeSwap' :: V.Vector Word16 -> Char -> Char -> V.Vector Word16
executeSwap' ls c1 c2 =
  executeExchange ls (getIndexOfChar c1) (getIndexOfChar c2)
--  let i1 = fromJust (V.elemIndex c1 ls)
--      i2 = fromJust (V.elemIndex c2 ls)
--  in executeExchange' ls i1 i2

vectorToString :: V.Vector Word16 -> String
vectorToString ws = map (findCharForIndex ws) [0..(V.length ws - 1)]

findCharForIndex :: V.Vector Word16 -> Int -> Char
findCharForIndex ws n = getCharForIndex . fst . V.head . V.filter (flip testBit n . snd) . V.indexed $ ws

getCharForIndex :: Int -> Char
getCharForIndex = chr . (ord 'a' +)

stringToVector :: String -> V.Vector Word16
stringToVector s = V.generate (length s) (getPositionForChar s)

getIndexOfChar :: Char -> Int
getIndexOfChar c = ord c - ord 'a'

getPositionForChar :: String -> Int -> Word16
--getPositionForChar s n | trace ("string: " ++ s ++ ", n: " ++ show n) False = undefined
getPositionForChar s n = bit . fromJust $ elemIndex (getCharForIndex n) s

-------------------------------------------------------------

sampleInput :: [Move]
sampleInput = [ Spin 1
              , Exchange 3 4
              , Swap 'e' 'b'
              ]

--abcdefghijklmnop
--pabcdefghijklmno
--pabdcefghijklmno
--paedcbfghijklmno


tests :: IO ()
tests = defaultMain $ testGroup "Day 16"
  [ stringToVectorTests
  , spinTests
  , exchangeTests
  , swapTests
  , doDanceTests
  , doMultipleDancesTests
  ]
 where
   stringToVectorTests = testGroup "string to word vector"
     [ testCase "abc" $ stringToVector "abc" @?= V.fromList [bit 0, bit 1, bit 2]
     ]
   spinTests = testGroup "spin"
     [ testCase "shorter" $  vectorToString (executeSpin' getBitList 3) @?= "nopabcdefghijklm"
     , testCase "longer" $  vectorToString (executeSpin' getBitList 10) @?= "ghijklmnopabcdef"
     ]
   exchangeTests = testGroup "exchange"
     [ testCase "adjacent normal" $ vectorToString (executeExchange' getBitList 3 4) @?= "abcedfghijklmnop"
     , testCase "separated normal" $ vectorToString (executeExchange' getBitList 1 3) @?= "adcbefghijklmnop"
     , testCase "backwards" $ vectorToString (executeExchange' getBitList 3 1) @?= "adcbefghijklmnop"
     , testCase "adjacent mixed" $ vectorToString (executeExchange' (stringToVector "efghijklmnopabcd") 3 4)  @?= "efgihjklmnopabcd"
     ]
   swapTests = testGroup "swap"
     [ testCase "adjacent" $ executeSwap' getBitList 'd' 'e' @?= stringToVector "abcedfghijklmnop"
     , testCase "separated" $ executeSwap' getBitList 'b' 'd' @?= stringToVector "adcbefghijklmnop"
     , testCase "backwards" $ executeSwap' getBitList 'd' 'b' @?= stringToVector "adcbefghijklmnop"
     ]
   doDanceTests = testGroup "do 1 dance"
     [ testCase "sample input" $ vectorToString (doDance' getBitList sampleInput) @?= "paedcbfghijklmno"
     , testCase "real input" $ part1 >>= (@?= "fgmobeaijhdpkcln")
     ]
   doMultipleDancesTests = testGroup "do multiple dances"
     [ --testCase "sampleInput" $ doMultipleDances 2 (V.fromList ['a'..'e']) sampleInput @?= V.fromList "ceadb"
     ]


--   spinTest = testCase "spin" $  executeSpin "abcde" 3 @?= "cdeab"
--   exchangeTests = testGroup "exchange"
--     [ testCase "adjacent" $ executeExchange "abcde" 3 4  @?= "abced"
--     , testCase "separated" $ executeExchange "abcde" 1 3  @?= "adcbe"
--     , testCase "backwards" $ executeExchange "abcde" 3 1  @?= "adcbe" ]
--   swapTests = testGroup "swap"
--     [ testCase "adjacent" $ executeSwap "abcde" 'd' 'e' @?= "abced"
--     , testCase "separated" $ executeSwap "abcde" 'b' 'd' @?= "adcbe"
--     , testCase "backwards" $ executeSwap "abcde" 'd' 'b' @?= "adcbe" ]
--   doDanceTests = testGroup "do 1 dance"
--     [ testCase "sample input" $ doDance ['a'..'e'] [ Spin 1
--                                                    , Exchange 3 4
--                                                    , Swap 'e' 'b' ] @?= "baedc"
--     , testCase "real input" $ part1 >>= (@?= "fgmobeaijhdpkcln")
--     ]
--   doMultipleDancesTests = testGroup "do multiple dances"
--     [ testCase "sampleInput" $ doMultipleDances 2 ['a'..'e'] [ Spin 1
--                                                              , Exchange 3 4
--                                                              , Swap 'e' 'b' ] @?= "ceadb"
--     ]
