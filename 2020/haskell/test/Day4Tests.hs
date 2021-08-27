{-# LANGUAGE OverloadedStrings #-}
module Day4Tests where

import Day4
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers

import qualified Data.Text as T

day4 :: TestTree
day4 = testGroup "day 4"
  [ testCase "part 1" $ part1 >>= (@?= 200)
  , testCase "part 2" $ part2 >>= (@?= 116)
  , testGroup "parser"
    [ testCase "passport parser" $ 
      testParse passportParser passportInput @?= passportOutput
    , testCase "multiple passport parser" $ 
        length (testParse inputParser (passportInput <> "\n\n" <> passportInput)) @?= 2
    , testCase "passport with newline" $ 
        testParse inputParser (T.replace " " "\n" passportInput) @?= [passportOutput]
    , testCase "parses input" $
      readInput >>= (@? "Input should not be empty") . not . null
    ]
  , testGroup "birth year"
    [ testCase "too low" $
      testParse inputParser "byr:1919" @?= [passportWithBirthYear (Left $ Just "1919")]
    , testCase "lowest valid" $
      testParse inputParser "byr:1920" @?= [passportWithBirthYear (Right 1920)]
    , testCase "highest valid" $
      testParse inputParser "byr:2002" @?= [passportWithBirthYear (Right 2002)]
    , testCase "too high" $
      testParse inputParser "byr:2003" @?= [passportWithBirthYear (Left $ Just "2003")]
    ]
  , testGroup "issue year"
    [ testCase "too low" $
      testParse inputParser "iyr:2009" @?= [passportWithIssueYear (Left $ Just "2009")]
    , testCase "lowest valid" $
      testParse inputParser "iyr:2010" @?= [passportWithIssueYear (Right 2010)]
    , testCase "highest valid" $
      testParse inputParser "iyr:2020" @?= [passportWithIssueYear (Right 2020)]
    , testCase "too high" $
      testParse inputParser "iyr:2021" @?= [passportWithIssueYear (Left $ Just "2021")]
    ]
  , testGroup "expiration year"
    [ testCase "too low" $
      testParse inputParser "eyr:2019" @?= [passportWithExpirationYear (Left $ Just "2019")]
    , testCase "lowest valid" $
      testParse inputParser "eyr:2020" @?= [passportWithExpirationYear (Right 2020)]
    , testCase "highest valid" $
      testParse inputParser "eyr:2030" @?= [passportWithExpirationYear (Right 2030)]
    , testCase "too high" $
      testParse inputParser "eyr:2031" @?= [passportWithExpirationYear (Left $ Just "2031")]
    ]
  , testGroup "height"
    [ testGroup "inches"
      [ testCase "too low" $
        testParse inputParser "hgt:58in" @?= [passportWithHeight (Left $ Just "58in")]
      , testCase "lowest valid" $
        testParse inputParser "hgt:59in" @?= [passportWithHeight (Right (Height Inches 59))]
      , testCase "highest valid" $
        testParse inputParser "hgt:76in" @?= [passportWithHeight (Right (Height Inches 76))]
      , testCase "too high" $
        testParse inputParser "hgt:77in" @?= [passportWithHeight (Left $ Just "77in")]
      ]
    , testGroup "centimeters"
      [ testCase "too low" $
        testParse inputParser "hgt:149cm" @?= [passportWithHeight (Left $ Just "149cm")]
      , testCase "lowest valid" $
        testParse inputParser "hgt:150cm" @?= [passportWithHeight (Right (Height Centimeters 150))]
      , testCase "highest valid" $
        testParse inputParser "hgt:193cm" @?= [passportWithHeight (Right (Height Centimeters 193))]
      , testCase "too high" $
        testParse inputParser "hgt:194cm" @?= [passportWithHeight (Left $ Just "194cm")]
      ]
    ]
  , testGroup "hair color"
    [ testCase "valid" $
      testParse inputParser "hcl:#123aff" @?= [passportWithHairColor (Right (HairColor "123aff"))]
    , testCase "invalid char" $
      testParse inputParser "hcl:#abcdeg" @?= [passportWithHairColor (Left (Just "#abcdeg"))]
    , testCase "valid for all characters a-f" $
      testParse inputParser "hcl:#abcdef" @?= [passportWithHairColor (Right (HairColor "abcdef"))]
    , testCase "missing #" $
      testParse inputParser "hcl:123aff" @?= [passportWithHairColor (Left (Just "123aff"))]
    , testCase "too many characters" $
      testParse inputParser "hcl:#1234567" @?= [passportWithHairColor (Left (Just "#1234567"))]
    , testCase "invalid char" $
      testParse inputParser "hcl:#abcdeg" @?= [passportWithHairColor (Left (Just "#abcdeg"))]
    ]
  , testGroup "eye color"
    [ testCase "amber" $
      testParse inputParser "ecl:amb" @?= [passportWithEyeColor (Right Amber)]
    , testCase "blue" $
      testParse inputParser "ecl:blu" @?= [passportWithEyeColor (Right Blue)]
    , testCase "brown" $
      testParse inputParser "ecl:brn" @?= [passportWithEyeColor (Right Brown)]
    , testCase "gray" $
      testParse inputParser "ecl:gry" @?= [passportWithEyeColor (Right Gray)]
    , testCase "green" $
      testParse inputParser "ecl:grn" @?= [passportWithEyeColor (Right Green)]
    , testCase "hazel" $
      testParse inputParser "ecl:hzl" @?= [passportWithEyeColor (Right Hazel)]
    , testCase "other" $
      testParse inputParser "ecl:oth" @?= [passportWithEyeColor (Right Other)]
    , testCase "invalid" $
      testParse inputParser "ecl:pur" @?= [passportWithEyeColor (Left (Just "pur"))]
    , testCase "invalid - too long" $
      testParse inputParser "ecl:purp" @?= [passportWithEyeColor (Left (Just "purp"))]
    ]
  , testGroup "passport id"
    [ testCase "valid" $
      testParse inputParser "pid:012345678" @?= [passportWithPassportId (Right (PassportId "012345678"))]
    , testCase "too short" $
      testParse inputParser "pid:01234567" @?= [passportWithPassportId (Left (Just "01234567"))]
    , testCase "too long" $
      testParse inputParser "pid:0123456789" @?= [passportWithPassportId (Left (Just "0123456789"))]
    , testCase "failes if has letters" $
      testParse inputParser "pid:a12345678" @?= [passportWithPassportId (Left (Just "a12345678"))]
    ]
  ]

passportInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

passportOutput :: Passport
passportOutput = 
      Passport 
        { eyeColor = Right Gray
        , passportId = Right (PassportId "860033327")
        , expirationYear = Right 2020
        , hairColor = Right (HairColor "fffffd")
        , birthYear = Right 1937
        , issueYear = Right 2017
        , countryId = Right "147"
        , height = Right (Height Centimeters 183)
        }

passportWithBirthYear :: Field Int -> Passport
passportWithBirthYear byr = Passport byr (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing)

passportWithIssueYear :: Field Int -> Passport
passportWithIssueYear iyr = Passport (Left Nothing) iyr (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing)

passportWithExpirationYear :: Field Int -> Passport
passportWithExpirationYear eyr = Passport (Left Nothing) (Left Nothing) eyr (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing)

passportWithHeight :: Field Height -> Passport
passportWithHeight hgt = Passport (Left Nothing) (Left Nothing) (Left Nothing) hgt (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing)

passportWithHairColor :: Field HairColor -> Passport
passportWithHairColor hcl = Passport (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) hcl (Left Nothing) (Left Nothing) (Left Nothing)

passportWithEyeColor :: Field EyeColor -> Passport
passportWithEyeColor ecl = Passport (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) ecl (Left Nothing) (Left Nothing)

passportWithPassportId :: Field PassportId -> Passport
passportWithPassportId pid = Passport (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) (Left Nothing) pid (Left Nothing)

test :: T.Text
test = "byr:1937\n\
\eyr:2030 pid:154364481\n\
\hgt:158cm iyr:2015 ecl:brn hcl:#c0946f cid:155\n\
\\n\
\cid:279\n\
\eyr:2029\n\
\pid:675014709 ecl:amb\n\
\byr:1985 hgt:179in hcl:z iyr:2025"