{-# LANGUAGE OverloadedStrings #-}
module Day1 (result) where

import           Data.Char        (digitToInt)
import           Data.List        (inits, tails)
import           Data.Maybe
import qualified Data.Text        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           DayData

result :: IO Day
result = return $ Day (part1Alg input) (part2Alg input)

part1Alg :: String -> Result
part1Alg s = sumDigits $ filterForMatch s (rotateBy 1 s)

sumDigits :: String -> String
sumDigits = show . sum . map digitToInt

rotateBy :: Int -> String -> String
rotateBy n s = let newBeginChars = reverse . take n . reverse $ s
                   newEndChars = take (length s - n) s
               in newBeginChars ++ newEndChars
-- rotateBy n s = let allRotatations = reverse $ zipWith (++) (tails s) (inits s)
--                in allRotatations !! n

part2Alg :: String -> Result
part2Alg s = let halfOfLength = length s `quot` 2
                 halfRotation = rotateBy halfOfLength s
             in sumDigits (filterForMatch halfRotation s)

filterForMatch :: String -> String -> String
filterForMatch s1 s2 = mapMaybe checkForMatch $ zip s1 s2
-- filterForMatch = (mapMaybe checkForMatch .) . zip
  where
    checkForMatch (a,b) = if a == b then Just a else Nothing

tests :: IO ()
tests = defaultMain $ testGroup "Tests"
  [ rotateByTests
  , filterForMatchTests
  , sumDigitsTests
  , part1AlgTests
  , part2AlgTests
  ]
  where
    rotateByTests = testGroup "rotateBy"
      [ testCase "1" $ rotateBy 1 "1234" @?= "4123"
      , testCase "2" $ rotateBy 2 "1234" @?= "3412"
      , testCase "3" $ rotateBy 3 "1234" @?= "2341"
      , testCase "4" $ rotateBy 4 "1234" @?= "1234"
      ]

    filterForMatchTests = testGroup "filterForMatch"
      [ testCase "no matches" $ filterForMatch "4123" "1234" @?= ""
      , testCase "1 match" $ filterForMatch "3122" "1223" @?= "2"
      , testCase "first and end match" $ filterForMatch "1123" "1231" @?= "1"
      , testCase "all match" $ filterForMatch "1111" "1111" @?= "1111"
      ]

    sumDigitsTests = testGroup "sumDigits"
      [ testCase "empty" $ sumDigits "" @?= "0"
      , testCase "single digit" $ sumDigits "5" @?= "5"
      , testCase "multiple digits" $ sumDigits "12345" @?= "15"
      ]

    part1AlgTests = testGroup "part 1 algorithm tests"
      [ testCase "10" $ part1Alg "10" @?= "0"
      , testCase "11" $ part1Alg "11" @?= "2"
      , testCase "101" $ part1Alg "101" @?= "1"
      , testCase "1122" $ part1Alg "1122" @?= "3"
      , testCase "1111" $ part1Alg "1111" @?= "4"
      , testCase "1234" $ part1Alg "1234" @?= "0"
      , testCase "91212129" $ part1Alg "91212129" @?= "9"
      , testCase "full input" $ part1Alg input @?= "1393"
      ]

    part2AlgTests = testGroup "part 2 algorithm tests"
      [ testCase "1212" $ part2Alg "1212" @?= "6"
      , testCase "1221" $ part2Alg "1221" @?= "0"
      , testCase "123425" $ part2Alg "123425" @?= "4"
      , testCase "123123" $ part2Alg "123123" @?= "12"
      , testCase "12131415" $ part2Alg "12131415" @?= "4"
      , testCase "full input" $ part2Alg input @?= "1292"
      ]

input :: String
input = "5994521226795838486188872189952551475352929145357284983463678944777228139398117649129843853837124228353689551178129353548331779783742915361343229141538334688254819714813664439268791978215553677772838853328835345484711229767477729948473391228776486456686265114875686536926498634495695692252159373971631543594656954494117149294648876661157534851938933954787612146436571183144494679952452325989212481219139686138139314915852774628718443532415524776642877131763359413822986619312862889689472397776968662148753187767793762654133429349515324333877787925465541588584988827136676376128887819161672467142579261995482731878979284573246533688835226352691122169847832943513758924194232345988726741789247379184319782387757613138742817826316376233443521857881678228694863681971445442663251423184177628977899963919997529468354953548612966699526718649132789922584524556697715133163376463256225181833257692821331665532681288216949451276844419154245423434141834913951854551253339785533395949815115622811565999252555234944554473912359674379862182425695187593452363724591541992766651311175217218144998691121856882973825162368564156726989939993412963536831593196997676992942673571336164535927371229823236937293782396318237879715612956317715187757397815346635454412183198642637577528632393813964514681344162814122588795865169788121655353319233798811796765852443424783552419541481132132344487835757888468196543736833342945718867855493422435511348343711311624399744482832385998592864795271972577548584967433917322296752992127719964453376414665576196829945664941856493768794911984537445227285657716317974649417586528395488789946689914972732288276665356179889783557481819454699354317555417691494844812852232551189751386484638428296871436139489616192954267794441256929783839652519285835238736142997245189363849356454645663151314124885661919451447628964996797247781196891787171648169427894282768776275689124191811751135567692313571663637214298625367655969575699851121381872872875774999172839521617845847358966264291175387374464425566514426499166813392768677233356646752273398541814142523651415521363267414564886379863699323887278761615927993953372779567675"
