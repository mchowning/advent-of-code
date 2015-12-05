{-# OPTIONS_GHC -Wall -Werror #-}

module Day4( result1
           , result2
           , tests
           ) where

import Data.List
import Test.HUnit

import Data.Hash.MD5 as M

result1 :: (Int,String)
result1 = result input "00000"

result2 :: (Int,String)
result2 = result input "000000"

result :: String -> String -> (Int,String)
result startPrefix endPrefix = head . filter (isPrefixOf endPrefix . snd) . map (md5ForString startPrefix) $ [0..]

md5ForString :: String -> Int -> (Int,String)
md5ForString str n = (n, M.md5s (M.Str (str ++ (show n))))

input :: String
input = "bgvyzdsv"

-- Caution: these tests are slow
tests :: IO Counts
tests = runTestTT $ TestList [ fst (result "abcdef" "00000") ~?= 609043 
                             , fst (result "pqrstuv" "00000") ~?= 1048970
                             ]   
