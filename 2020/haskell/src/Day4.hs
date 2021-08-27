{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Day4 where

import Util
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict ((!?))
import Control.Monad
import Data.Either.Combinators
import Data.Functor
import Data.Maybe
import Data.Bifunctor
import qualified Data.Text as T

-- newtype Passport = Passport [Field] deriving (Eq, Show)
-- type Passport = Map.HashMap String (Maybe String)
newtype HairColor = HairColor String deriving (Eq, Show)
data EyeColor = Amber
              | Blue
              | Brown
              | Gray
              | Green
              | Hazel
              | Other
              deriving (Eq, Show)
-- data FieldError = Missing 
--                 | CouldNotParse String 
--                 | Invalid String 
--                 deriving (Eq, Show)
newtype PassportId = PassportId String deriving (Eq, Show)
type Field a = Either (Maybe String) a -- TODO could drop the Either and just have a big union
data Passport = Passport {
  birthYear :: Field Int,
  issueYear :: Field Int,
  expirationYear :: Field Int,
  height :: Field Height,
  hairColor :: Field HairColor,
  eyeColor :: Field EyeColor,
  passportId :: Field PassportId,
  countryId :: Field String
} deriving (Eq, Show)

data Dimen = Centimeters | Inches deriving (Eq, Show)
data Height = Height Dimen Int deriving (Eq, Show)

readInput :: IO [Passport]
readInput = parseInput inputParser "../inputs/4.txt"

inputParser :: Parser [Passport]
inputParser = passportParser `sepBy1` count 2 eol

passportParser :: Parser Passport
passportParser = listToPassport <$> (keyValueParser `sepBy` singleSpace)
  where
    -- "space" includes newlines
    singleSpace :: Parser Char
    singleSpace = try (spaceChar <* notFollowedBy spaceChar)

    listToPassport :: [(String, String)]-> Passport
    -- listToPassport = Map.map Just . Map.fromList
    listToPassport ls = 
      let m = Map.fromList ls
      in Passport {
          birthYear      = extract "byr" m decimal (\n -> n >= 1920 && n <= 2002),
          issueYear      = extract "iyr" m decimal (\n -> n >= 2010 && n <= 2020),
          expirationYear = extract "eyr" m decimal (\n -> n >= 2020 && n <= 2030),
          height         = extract "hgt" m heightParser (const True),
          hairColor      = extract "hcl" m hairColorParser (const True),
          eyeColor       = extract "ecl" m eyeColorParser (const True),
          passportId     = extract "pid" m passportIdParser (const True),
          countryId      = maybeToRight Nothing (m !? "cid")
        }

    extract :: String 
            -> Map.HashMap String String 
            -> Parser a 
            -> (a -> Bool) 
            -> Field a
    extract key m parser predicate = 
      case m !? key of
        Nothing -> Left Nothing
        (Just value) -> 
          -- FIXME can I fix this pyramid of doom?
          case parse' parser (T.pack value) of
            (Left _) -> Left (Just value)
            (Right n) -> 
              if predicate n
                then Right n
                else Left (Just value)

    expirationYearParser :: Parser Int
    expirationYearParser = do
      n <- decimal
      if n `inRange` (2020, 2030)
        then return n
        else fail "invalid input"
    
    inRange :: Int -> (Int, Int) -> Bool
    inRange n (bottom, top) = n >= bottom && n <= top

    heightValidator :: Dimen -> Int -> Bool
    heightValidator dimen n = 
      let (bottom, top) = case dimen of
                            Inches -> (59, 76)
                            Centimeters -> (150, 193)
      in n >= bottom && n <= top

    heightParser :: Parser Height
    heightParser = do
      n <- decimal
      dimen <- (string "in" $> Inches) 
            <|> (string "cm" $> Centimeters)
      if heightValidator dimen n
        then return (Height dimen n)
        else fail ("Invalid height of " <> show n <> " " <> show dimen)
      return (Height dimen n)

    hairColorParser :: Parser HairColor
    hairColorParser = do
      char '#'
      hex <- count 6 hexDigitChar 
      eof
      return (HairColor hex)

    eyeColorParser :: Parser EyeColor
    eyeColorParser = some letterChar >>= \case
        "amb" -> return Amber
        "blu" -> return Blue
        "brn" -> return Brown
        "gry" -> return Gray
        "grn" -> return Green
        "hzl" -> return Hazel
        "oth" -> return Other
        _ -> fail "Invalid eye color"

    passportIdParser :: Parser PassportId
    passportIdParser = PassportId <$> count 9 digitChar <* eof
    -- do
    --   num <- count 9 digitChar 
    --   eof
    --   return (PassportId num)


    keyValueParser :: Parser (String, String)
    keyValueParser = do
      key <- some letterChar
      char ':'
      value <- some (alphaNumChar <|> char '#')
      return (key, value)

part1 :: IO Int
part1 = part1' <$> readInput

part2 :: IO Int
part2 = part2' <$> readInput

part1' :: [Passport] -> Int
part1' = length . filter isValidPassportPart1

part2' :: [Passport] -> Int
part2' = length . filter isValidPassportPart2

isValidPassportPart1 :: Passport -> Bool
isValidPassportPart1 p = and [ hasValue birthYear
                             , hasValue issueYear 
                             , hasValue expirationYear
                             , hasValue height
                             , hasValue hairColor
                             , hasValue eyeColor
                             , hasValue passportId]
  where
    hasValue :: (Passport -> Field a) -> Bool
    hasValue f = case f p of
      (Left Nothing) -> False
      _ -> True


isValidPassportPart2 :: Passport -> Bool
isValidPassportPart2 p = and [ hasValidValue birthYear
                             , hasValidValue issueYear 
                             , hasValidValue expirationYear
                             , hasValidValue height
                             , hasValidValue hairColor
                             , hasValidValue eyeColor
                             , hasValidValue passportId]
  where
    hasValidValue :: (Passport -> Field a) -> Bool
    hasValidValue f = case f p of
      (Right _) -> True
      _ -> False