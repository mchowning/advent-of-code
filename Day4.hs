{-# OPTIONS_GHC -Wall -Werror #-}

module Day4( result1
           , result2
           , tests
           ) where

import Data.List
import Test.HUnit

import qualified Data.Hash.MD5 as M

result1 :: (Int,String)
result1 = result input "00000"

result2 :: (Int,String)
result2 = result input "000000"

result :: String -> String -> (Int,String)
result startPrefix desiredPrefix = head . filter (isPrefixOf desiredPrefix . snd) . map (md5ForString startPrefix) $ [0..]
-- result startPrefix desiredPrefix = head [ pair | (md5ForString startPrefix -> pair@(_, md5Hash)) <- [0..], desiredPrefix `isPrefixOf` md5Hash ]

md5ForString :: String -> Int -> (Int,String)
md5ForString str n = let md5String = M.md5s (M.Str $ str ++ show n)
                     in (n, md5String)

input :: String
input = "bgvyzdsv"

-- Caution: these tests are slow
tests :: IO Counts
tests = runTestTT $ TestList [ fst (result "abcdef" "00000") ~?= 609043 
                             , fst (result "pqrstuv" "00000") ~?= 1048970
                             ]   

-- just playing with ViewPatterns
-- {-# LANGUAGE ViewPatterns #-}
-- capitalize :: String -> String
-- capitalize (x:xs) | x' <- toUpper x = x' : xs
-- capitalize ((toUpper -> x):(id -> xs)) = x:xs
-- capitalize (x:xs) = let x' = toUpper x in x' : xs

