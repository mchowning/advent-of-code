{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day6 (part1, part2) where

import           Data.Maybe       (fromJust)
import qualified Data.Set         as St
import qualified Data.Sequence    as Sq

import           Test.Tasty
import           Test.Tasty.HUnit

type Index = Int
type Block = Integer
newtype Bank = Bank (Sq.Seq Block) deriving (Eq, Show, Ord)

input :: IO Bank
input = Bank . fmap read . Sq.fromList . words <$> readFile "src/input_day6.txt"

part1 :: IO Int
part1 = part1Algo <$> input

part2 :: IO Int
part2 = part2Algo <$> input

part1Algo :: Bank -> Int
part1Algo = roundsToDupliate St.empty

part2Algo :: Bank -> Int
part2Algo = lengthOfCycle []

-- infiniteList :: Bank -> [Bank]
-- infiniteList = appendToList []
--   where
--     appendToList bs b = appendToList (b : bs) (bankUpdate b)

roundsToDupliate :: St.Set Bank -> Bank -> Int
roundsToDupliate st b = if St.member b st
                           then St.size st
                           else roundsToDupliate (St.insert b st) (bankUpdate b)

lengthOfCycle :: [Bank] -> Bank -> Int
lengthOfCycle bs b = if b `elem` bs
                       then length . dropWhile (/= b) . reverse $ bs
                       else lengthOfCycle (b : bs) (bankUpdate b)

blockWalker :: Sq.Seq Block -> Index -> Integer -> Sq.Seq Block
blockWalker bs i amount =
  if amount == 0 then bs
  else let wrappedIndex = if i == Sq.length bs then 0 else i
           currentValue = Sq.index bs wrappedIndex
           newSeq = Sq.update wrappedIndex (1+currentValue) bs
       in blockWalker newSeq (wrappedIndex+1) (amount-1)

bankUpdate :: Bank -> Bank
bankUpdate (Bank b) = let sortedDesc = Sq.sortBy (flip compare) b
                          maxValue = Sq.index sortedDesc 0
                          maxIndex = fromJust (Sq.elemIndexL maxValue b)
                          bsWithIndexCleared = Sq.update maxIndex 0 b
                       in Bank $ blockWalker bsWithIndexCleared (maxIndex+1) maxValue

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ bankUpdateTest
  , part1AlgoTests
  , part2AlgoTests
  ]
  where
    part1AlgoTests = testGroup "part1Algo"
      [ testCase "[0,2,7,0]" $ part1Algo (Bank (Sq.fromList [0,2,7,0])) @?= 5
      , testCase "test input" $ do content <- input
                                   part1Algo content @?= 3156
      ]
    part2AlgoTests = testGroup "part2Algo"
      [ testCase "[0,2,7,0]" $ part2Algo (Bank (Sq.fromList [0,2,7,0])) @?= 4
      -- , testCase "test input" $ do content <- input
      --                              part1Algo content @?= _
      ]
    bankUpdateTest = testCase "bankUpdate test" $
      bankUpdate (Bank (Sq.fromList [0,2,7,0])) @?= Bank (Sq.fromList [2,4,1,2])
