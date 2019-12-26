module Day16 where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Primitive as V

type Index = Int

input :: IO (V.Vector Int)
input = V.fromList . fmap digitToInt <$> readFile "../inputs/day16.txt"

-- 58672132
part1 :: IO (V.Vector Int)
part1 = part1' <$> input

part1' :: V.Vector Int -> V.Vector Int
part1' = afterPhase 100

afterPhase :: Int -> V.Vector Int -> V.Vector Int
afterPhase n = (!! n) . iterate runPhase

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

patternForIndex :: Int -> [Int]
patternForIndex = tail . patternForIndex'
  where
    patternForIndex' n = cycle (concatMap (replicate (n+1)) basePattern)

onesDigit :: Int -> Int
onesDigit = (`mod` 10) . abs

calculateIndex :: V.Vector Int -> Index -> Int
calculateIndex is index =
  let iValue = fromMaybe (error ("invalid index of " <> show index <> " for vector of length " <> show (V.length is)))
                 (is V.!? index)
      pattern = patternForIndex index
      products = zipWith (*) (V.toList is) pattern
  in onesDigit (sum products)
  
runPhase :: V.Vector Int -> V.Vector Int
runPhase is = V.fromList (calculateIndex is <$> take (V.length is) [0..])
