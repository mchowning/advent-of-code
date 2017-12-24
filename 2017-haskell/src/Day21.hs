{-# LANGUAGE TupleSections, BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
module Day21 (part1, part2) where

import           Data.Functor         (($>))
import qualified Data.Map.Strict      as M
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Test.Tasty
import           Test.Tasty.HUnit
import Data.List (transpose)
import qualified Data.Set as S

newtype Pixel = Pixel { isOn :: Bool } deriving (Eq, Ord)
type Graph = [[Pixel]]

instance Show Pixel where
  show (Pixel True)  = "#"
  show (Pixel False) = "."

part1, part2 :: IO Int
part1 = numPixelsOnAfterN 5 <$> input
part2 = numPixelsOnAfterN 18 <$> input

numPixelsOnAfterN :: Int -> M.Map Graph Graph -> Int
numPixelsOnAfterN n = length . filter isOn . concat . processGraphNTimes n startPattern

startPattern = revPrettyPrint [".#.", "..#", "###"]

processGraphNTimes :: Int -> Graph -> M.Map Graph Graph -> Graph
processGraphNTimes n g m = iterate (processGraph m) g !! n

processGraph :: M.Map Graph Graph -> Graph -> Graph
processGraph m g =
  let subGraphs = if length g `mod` 2 == 0
                   then divideGraphBy 2 g
                   else divideGraphBy 3 g
--      !_ = trace ("subGraphs: :" ++ show subGraphs) False
      processedGraphs = map (map ((M.!) m)) subGraphs
  in combineGraphs processedGraphs

combineGraphs :: [[Graph]] -> Graph
combineGraphs = concatMap (map concat . transpose)

divideGraphBy :: Int -> Graph -> [[Graph]]
--divideGraphBy :: Int -> [[a]] -> [[[[a]]]]
divideGraphBy n = map (map transpose . takeGroup . transpose) . takeGroup
 where
  takeGroup :: [a] -> [[a]]
  takeGroup = reverse . takeGroup' . ([],)

  takeGroup' :: ([[a]],[a]) -> [[a]]
  takeGroup' (acc,[]) = acc
  takeGroup' (acc,rest) = let (g, rest') = splitAt n rest
                        in takeGroup' (g:acc, rest')

----------------------------------------------------------------------------------------------------


input :: IO (M.Map Graph Graph)
input = mapAllKeyPermutationsToValues <$> parseKeyValuePairs
 where
 
  parseKeyValuePairs :: IO [(Graph, Graph)]
  parseKeyValuePairs = processEither . parse fileParser filename <$> readFile filename
   where
    filename = "src/input_day21.txt"
    processEither (Left  e ) = error (parseErrorPretty e)
    processEither (Right rs) = rs
    
    fileParser :: Parsec Void String [(Graph, Graph)]
    fileParser = line `sepBy` eol
     where
      line = do
        key <- graph
        _ <- string " => "
        value <- graph
        return (key, value)

      graph = aRow `sepBy1` char '/'
      aRow = some pixel
      pixel = on <|> off
      on = char '#' $> Pixel True
      off = char '.' $> Pixel False
 
mapAllKeyPermutationsToValues :: [(Graph, Graph)] -> M.Map Graph Graph
mapAllKeyPermutationsToValues = M.fromList . S.toList . S.unions . map withAllKeyPermutations
-- where
withAllKeyPermutations :: (Graph, Graph) -> S.Set (Graph, Graph)
withAllKeyPermutations (k,v) = S.map (,v) (flipsAndRotations k)

flipsAndRotations = S.fromList . concatMap flips . rotations

rotations g = [ g
              , rotate90 g
              , iterate rotate90 g !! 2
              , iterate rotate90 g !! 3 ]
rotate90 = transpose . reverse

flips g = [g, flipVertical g, flipHorizontal g]
flipHorizontal = map reverse
flipVertical = reverse

----------------------------------------------------------------------------------------------------

prettyPrint :: Graph -> [String]
prettyPrint = map (map (\c -> if show c == "#" then '#' else '.'))

revPrettyPrint :: [String] -> Graph
revPrettyPrint = map (map (Pixel . (== '#')))

sampleGraphMap :: M.Map Graph Graph
sampleGraphMap = mapAllKeyPermutationsToValues [(revPrettyPrint ["..", ".#"], revPrettyPrint ["##.", "#..", "..."])
                            ,(revPrettyPrint [".#.", "..#", "###"], revPrettyPrint ["#..#", "....", "....", "#..#"])
                            ]

-- TODO use quickcheck to verify that rotate+rotate == flip+flip, among other things

testSample = ["abcd", "efgh", "ijkl", "mnop"]
sample2Graph = revPrettyPrint ["abcd", "efgh", "ijkl", "mnop"]
--sample3Graph = [[0..5], [6..11], [12..17], [18..23], [24..29], [30..35]]

tests = defaultMain $ testGroup "Day 21"
  [ testCase "divide graph into 2s" $
      divideGraphBy 2 sample2Graph @?= map (map revPrettyPrint) [ [ [ "ab"
                                                                    , "ef" ]
                                                                  , [ "cd"
                                                                    , "gh" ]
                                                                  ]
                                                                , [ [ "ij"
                                                                    , "mn" ]
                                                                  , [ "kl"
                                                                    , "op" ]
                                                                  ]
                                                                ]
  , testCase "divide and combine graph" $
      combineGraphs (divideGraphBy 2 sample2Graph) @?= sample2Graph
  , testCase "process graph" $
      processGraph sampleGraphMap (revPrettyPrint [".#.", "..#", "###"]) @?=
        revPrettyPrint ["#..#", "....", "....", "#..#"]
  , testCase "process graph" $
      processGraphNTimes 2 (revPrettyPrint [".#.", "..#", "###"]) sampleGraphMap @?=
        revPrettyPrint ["##.##.", "#..#..", "......", "##.##.", "#..#..", "......"]
  , testCase "part 1" $ part1 >>= (@?= 203)
  ]
-- where
