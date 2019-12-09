{-# LANGUAGE OverloadedStrings #-}
module Day8 where

import           Util

import           Control.Monad        (join, when)
import           Data.List            (minimumBy, transpose)
import           Data.List.Split      (chunksOf)
import           Data.Ord             (comparing)

import           Text.Megaparsec      (many)
import           Text.Megaparsec.Char (digitChar)

import           Data.Char            (digitToInt)

-- each digit represents color of a single pixel
-- layer is 25x6
-- answer is "the number of 1 digits multiplied by the number of 2 digits" on the layer with the fewest 0 digits

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [Int] -> Int
part1' = onesTimesTwos . minimumBy (comparing (count 0)) . makeLayers

onesTimesTwos :: [Int] -> Int
onesTimesTwos xs = count 1 xs * count 2 xs

count :: Int -> [Int] -> Int
count x = length . filter (== x)

makeLayers :: [a] -> [[a]]
makeLayers = chunksOf (25 * 6)

------------------------------------------------------------

-- 0 = black
-- 1 = white
-- 2 = transparent
-- first layer rendered in front and last layer in back

data Pixel = Black | White | Transparent deriving (Eq, Show)

instance Semigroup Pixel where
  Transparent <> p = p
  p <> _ = p

instance Monoid Pixel where
  mempty = Transparent

part2 :: Bool -> IO [Pixel]
part2 shouldPrint = do
  result <- part2' <$> readInput
  when shouldPrint (showMessage result)
  return result

part2' :: [Int] -> [Pixel]
part2' = map mconcat . transpose . makeLayers . map toPixel

toPixel :: Int -> Pixel
toPixel 0 = Black
toPixel 1 = White
toPixel 2 = Transparent
toPixel n = error ("unexpected pixel value: " <> show n)

showMessage :: [Pixel] -> IO ()
showMessage [] = return ()
showMessage xs = do
  let (line, rest) = splitAt 25 xs
  putStrLn . join $ (\p -> if p == White then "XX" else "  ") <$> line
  showMessage rest

readInts :: [Int] -> String
readInts = join . map show

------------------------------------------------------------

readInput :: IO [Int]
readInput = parseInput (many singleDigit) "day8.txt"

singleDigit :: Parser Int
singleDigit = digitToInt <$> digitChar
