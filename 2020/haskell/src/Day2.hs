
{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Util (parseInput, Parser)

import Text.Megaparsec (sepBy1, some)
import Text.Megaparsec.Char (eol, char, space1, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

data Input = Input { low :: Int
                   , high :: Int
                   , letter :: Char
                   , pwd :: String 
                   } deriving (Eq, Show)

lineParser :: Parser Input
lineParser = do
    low <- decimal
    char '-'
    high <- decimal
    space1
    letter <- letterChar
    char ':'
    space1
    pwd <- some letterChar
    return (Input low high letter pwd)

readInput :: IO [Input]
readInput = parseInput (lineParser `sepBy1` eol) "../inputs/2.txt"

----------------------------------------------------------------------------------

day2part1 :: IO Int
day2part1 = count isValid1 <$> readInput

day2part2 :: IO Int
day2part2 = count isValid2 <$> readInput

count :: (Input -> Bool) -> [Input] -> Int
count predicate = length . filter predicate

isValid1 :: Input -> Bool
isValid1 (Input low' high' c' pwd') =
    let count = length (filter (== c') pwd')
    in count >= low' && count <= high'

isValid2 :: Input -> Bool
isValid2 (Input i1 i2 c' pwd') =
    let extracted = [ pwd' !! (i1 - 1)
                    , pwd' !! (i2 - 1) ]
    in (== 1) . length . filter (== c') $ extracted

---------------------------------------------------------------------------------

-- xor :: Bool -> Bool -> Bool
-- xor a b = (a && not b) ||
--           (b && not a)

-- isValid2 :: Input -> Bool
-- isValid2 (Input i1 i2 c' pwd') =
--     let isMatch i = pwd' !! (i - 1) == c'
--     in isMatch i1 `xor` isMatch i2