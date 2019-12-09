{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import           Util

import Control.Monad (void)
import Data.List (foldl')
import Data.Maybe (maybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Safe.Foldable (minimumMay)

import           Text.Megaparsec            (sepBy1, some)
import           Text.Megaparsec.Char       (alphaNumChar, char, eol)

-- FIXME Try using Data.Tree or some other pre-made graph structure

part1 :: IO Int
part1 = part1' <$> readInput

part1' :: [(Text, Text)] -> Int
part1' tup =
  let orbitMap = mapOfOrbits tup
  in sum . fmap (indirectOrbits orbitMap) $ M.keys orbitMap

mapOfOrbits :: [(Text,Text)] -> M.Map Text Text
mapOfOrbits = foldl' (\m (a,b) -> M.insert b a m) M.empty

mapOfOrbited :: [(Text,Text)] -> M.Map Text [Text]
mapOfOrbited = foldl' (\m (a,b) -> M.insertWith (++) a [b] m) M.empty

indirectOrbits :: M.Map Text Text -> Text -> Int
indirectOrbits m t = maybe 0 (\t' -> 1 + indirectOrbits m t') (m M.!? t)

-------------------------------------------------------------------

part2 :: IO (Maybe Int)
part2 = part2' <$> readInput

part2' :: [(Text, Text)] -> Maybe Int
part2' tup =
  let allPlanets = search (mapOfOrbits tup) (mapOfOrbited tup) S.empty "SAN" "YOU"
      transfers = subtract 2 <$> allPlanets -- subtract ends (2)
  in transfers

search :: M.Map Text Text -> M.Map Text [Text] -> S.Set Text -> Text -> Text -> Maybe Int
search orbitsMap orbitedMap visited destination current =
  if current == destination
    then Just 0
    else
      let orbits = maybe S.empty S.singleton (orbitsMap M.!? current)
          orbited = maybe S.empty S.fromList (orbitedMap M.!? current)
          options = S.union orbits orbited S.\\ visited
          newVisited = S.insert current visited
      in let remainingDistances = search orbitsMap orbitedMap newVisited destination <$> S.toList options
         in (+ 1) <$> minimumMay (catMaybes remainingDistances)

-------------------------------------------------------------------

readInput :: IO [(Text, Text)]
readInput = parseInput inputParser "day6.txt"

inputParser :: Parser [(Text, Text)]
inputParser = lineParser `sepBy1` eol

lineParser :: Parser (Text, Text)
lineParser = do
  left <- some alphaNumChar
  void $ char ')'
  right <- some alphaNumChar
  return (T.pack left, T.pack right)
