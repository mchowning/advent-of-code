{-# OPTIONS_GHC -Wall -Werror #-}

module Day11 where

import Data.Char
import Data.List
import Test.HUnit

input :: String
input = "hxbxwxba"

nextPass :: String -> String
nextPass = head . filter (\str -> containsSequenceOf 3 str && noInvalidChars str && twoPairs str) . drop 1 . iterate bumpChars

bumpChars :: String -> String
bumpChars = snd . foldr (\c (shouldBump,ls) -> if shouldBump then charBumper c ls else (False, c:ls)) (True, [])
  where
    charBumper :: Char -> String -> (Bool,String)
    charBumper c str = let bumped = bumpChar c
                           newStr = bumped:str
                           bumpNext = bumped == 'a'
                       in (bumpNext,newStr)

    bumpChar :: Char -> Char
    bumpChar 'z' = 'a'
    bumpChar c   = chr . (+1) . ord $ c
    --bumpChar = let dict = zip ['a'..'z'] ('z':['a'..'y']) in fromJust . flip lookup dict
    --bumpChar c   =  head . drop 1 $ [c..'z']
    --bumpChar = chr . (+ ord 'a') . (`mod` 26) . (+1) . subtract (ord 'a')  . ord 

containsSequenceOf :: Int -> String -> Bool
containsSequenceOf n = any isSequential . map (take n) . filter ((>=n) . length) . tails
  where
    isSequential :: String -> Bool
    isSequential str = str == take (length str) [head str..]

noInvalidChars :: String -> Bool
noInvalidChars str = all (`notElem` str) ['i','o','l']

-- Assumes that the two pairs must be different letters
twoPairs :: String -> Bool
twoPairs = (>1) . length . nub . map head . filter ((>1) . length) . group

-------------------------------------------------------------------------------
-- TESTS ----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO Counts
main = runTestTT $ TestList 
  [ containsSequenceOf 3 "abcee" ~?= True
  , containsSequenceOf 3 "abdde" ~?= False
  , containsSequenceOf 2 "abdde" ~?= True

  , noInvalidChars "abcdefghjkmnpqrstuvwxyz" ~?= True
  , noInvalidChars "i" ~?= False
  , noInvalidChars "o" ~?= False
  , noInvalidChars "l" ~?= False
  , noInvalidChars "hijklmn" ~?= False

  , twoPairs "abccdeef" ~?= True
  , twoPairs "abcdefgh" ~?= False
  , twoPairs "aaa"      ~?= False
  , twoPairs "aabaa"    ~?= False

  , bumpChars "aa" ~?= "ab"
  , bumpChars "az" ~?= "ba"

  , nextPass "abcdefgh"        ~?= "abcdffaa"
  --, nextPass "ghijklmn"        ~?= "ghjaabcc" -- slow
  , nextPass "ghizzzzz"        ~?= "ghjaabcc"
  , nextPass input            ~?= "hxbxxyzz"
  --, nextPass (nextPass input) ~?= "hxcaabcc" -- slow
  ]
