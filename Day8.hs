{-# OPTIONS_GHC -Wall -Werror #-}

module Day8 where

import Test.HUnit
import Text.Printf

results :: IO ()
results = do input <- readFile "day8_input.txt"
             printResult 1 (result1 input)
             printResult 2 (result2 input)
  where
    printResult :: Int -> String -> IO ()
    printResult = printf "result %d: %s\n"

-- 1333
result1 :: String -> String
result1 = show . sum . map decodedMemoryDecrease . lines

-- 2046
result2 :: String -> String
result2 = show . sum . map encodedMemoryIncrease . lines

-- TODO good place to work on learning monads
decodedMemoryDecrease :: String -> Int
decodedMemoryDecrease str = stringMemory str - decodedStringChars str

encodedMemoryIncrease :: String -> Int
encodedMemoryIncrease str = encodedStringChars str - length str

stringMemory :: String -> Int
stringMemory = (+2) . length 

decodedStringChars :: String -> Int
decodedStringChars = length . decode

encodedStringChars :: String -> Int
encodedStringChars = length . show

decode :: String -> String
decode ('\\':'\\':xs) = '\\' : decode xs
decode ('\\':'\"':xs) = "\"" ++ decode xs
decode ('\\':'x':_:_:xs) = "!" ++ decode xs
decode (x:xs) = x : decode xs
decode [] = []


-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ stringMemory ""                                  ~?= 2
  , stringMemory "abc"                               ~?= 5
  , stringMemory "aaa\\\"aaa"                        ~?= 10
  , stringMemory "\\x27"                             ~?= 6
  , stringMemory "f\\\"kak\\x70sn\\xc4kjri"          ~?= 22

  , decodedStringChars ""                            ~?= 0
  , decodedStringChars "abc"                         ~?= 3
  , decodedStringChars "aaa\\\"aaa"                  ~?= 7
  , decodedStringChars "\\x27"                       ~?= 1
  , decodedStringChars "f\\\"kak\\x70sn\\xc4kjri"    ~?= 13

  , decodedMemoryDecrease ""                         ~?= 2
  , decodedMemoryDecrease "abc"                      ~?= 2
  , decodedMemoryDecrease "aaa\\\"aaa"               ~?= 3
  , decodedMemoryDecrease "\\x27"                    ~?= 5
  , decodedMemoryDecrease "f\\\"kak\\x70sn\\xc4kjri" ~?= 9
  
  , encodedMemoryIncrease "\"\""                     ~?= 4
  , encodedMemoryIncrease "\"abc\""                  ~?= 4
  , encodedMemoryIncrease "\"aaa\\\"aaa\""           ~?= 6
  , encodedMemoryIncrease "\"\\x27\""                ~?= 5
  ]
