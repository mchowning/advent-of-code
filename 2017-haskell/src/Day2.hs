module Day2 where

import           Control.Applicative (liftA2)
import           DayData
import           Test.Tasty
import           Test.Tasty.HUnit

result :: IO Day
result = liftA2 Day (show <$> result1) (show <$> result2)

result1 :: IO Integer
result1 = do
  content <- readFile "src/input_day2.txt"
  return . checksum . parseIntegers $ content

result2 :: IO Integer
result2 = undefined

parseIntegers :: String -> [[Integer]]
parseIntegers = map (map read . words) . lines

checksum :: [[Integer]] -> Integer
checksum = sum . map rowChecksum

rowChecksum :: [Integer] -> Integer
rowChecksum ls = maximum ls - minimum ls

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ parseIntegersTests
  , rowChecksumTests
  , checksumTests
  ]
  where
    parseIntegersTests = testGroup "parseIntegers"
      [ testCase "5 1 9 5\n\
                 \7 5 3\n\
                 \2 4 6 8" $ parseIntegers "5 1 9 5\n\
                                          \7 5 3\n\
                                          \2 4 6 8" @?= [ [5,1,9,5]
                                                        , [7,5,3]
                                                        , [2,4,6,8]] ]
    rowChecksumTests = testGroup "rowChecksum"
      [ testCase "[1,4]" $ rowChecksum [1,4] @?= 3
      , testCase "[1,1,2,2,2,4,100]" $ rowChecksum [1,1,2,2,2,4,100] @?= 99 ]
    checksumTests = testGroup "checksum"
      [ testCase "[[5,1,9,5],[7,5,3],[2,4,6,8]]" $ checksum [[5,1,9,5],[7,5,3],[2,4,6,8]] @?= 18 ]





      -- [ result2Algo "[[5,9,2,8][9,4,7,3][3,8,6,5]]" $ result2Algo [ [5,9,2,8]
      --                                                       , [9,4,7,3]
      --                                                       , [3,8,6,5]] @?= 9 ]
