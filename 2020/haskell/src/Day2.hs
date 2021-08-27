{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Util (parseInput, Parser)

import Text.Megaparsec (sepBy1, some)
import Text.Megaparsec.Char (eol, char, space1, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

data Input = Input { inputLow :: Int
                   , inputHigh :: Int
                   , inputLetter :: Char
                   , inputPwd :: String 
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

inputParser :: Parser [Input]
inputParser = lineParser `sepBy1` eol

readInput :: IO [Input]
readInput = parseInput inputParser "../inputs/2.txt"

----------------------------------------------------------------------------------

-- benchmark: 1.5 ms ± 147 μs
part1 :: IO Int
part1 = count isValid1 <$> readInput

-- benchmark: 1.5 ms ±  79 μs
part2 :: IO Int
part2 = count isValid2 <$> readInput

isValid1 :: Input -> Bool
isValid1 (Input low high c pwd) =
    let num = count (== c) pwd
    in num >= low && num <= high

isValid2 :: Input -> Bool
isValid2 (Input i1 i2 c pwd) =
    let extracted = [ pwd !! (i1 - 1)
                    , pwd !! (i2 - 1) ]
    in (== 1) . count (== c) $ extracted


count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

---------------------------------------------------------------------------------

-- benchmark: 1.5 ms ± 111 μs
-- isValid2 :: Input -> Bool
-- isValid2 (Input i1 i2 c' pwd') =
--     let isMatch i = pwd' !! (i - 1) == c'
--     in isMatch i1 `xor` isMatch i2

-- xor :: Bool -> Bool -> Bool
-- xor a b = (a && not b) ||
--           (b && not a)