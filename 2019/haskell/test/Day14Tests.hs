{-# LANGUAGE OverloadedStrings #-}

module Day14Tests where

import Day14
import Util

import qualified Data.Map.Strict as M
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

example1_oreFor1Fuel = 31
example1 :: [Reaction]
example1 = parse' parser "example1" "10 ORE => 10 A\n\
                                    \1 ORE => 1 B\n\
                                    \7 A, 1 B => 1 C\n\
                                    \7 A, 1 C => 1 D\n\
                                    \7 A, 1 D => 1 E\n\
                                    \7 A, 1 E => 1 FUEL"

example2_oreFor1Fuel = 165
example2 :: [Reaction]
example2 = parse' parser "example2" "9 ORE => 2 A\n\
                                    \8 ORE => 3 B\n\
                                    \7 ORE => 5 C\n\
                                    \3 A, 4 B => 1 AB\n\
                                    \5 B, 7 C => 1 BC\n\
                                    \4 C, 1 A => 1 CA\n\
                                    \2 AB, 3 BC, 4 CA => 1 FUEL"

example3_oreFor1Fuel = 13312
example3_fuelForTrillionOre = 82892753
example3 :: [Reaction]
example3 = parse' parser "example3" "157 ORE => 5 NZVS\n\
                                    \165 ORE => 6 DCFZ\n\
                                    \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
                                    \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
                                    \179 ORE => 7 PSHF\n\
                                    \177 ORE => 5 HKGWZ\n\
                                    \7 DCFZ, 7 PSHF => 2 XJWVT\n\
                                    \165 ORE => 2 GPVTF\n\
                                    \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

example4_oreFor1Fuel = 180697
example4_fuelForTrillionOre = 5586022
example4 :: [Reaction]
example4 = parse' parser "example4" "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
                                    \17 NVRVD, 3 JNWZP => 8 VPVL\n\
                                    \53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
                                    \22 VJHF, 37 MNCFX => 5 FWMGM\n\
                                    \139 ORE => 4 NVRVD\n\
                                    \144 ORE => 7 JNWZP\n\
                                    \5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
                                    \5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
                                    \145 ORE => 6 MNCFX\n\
                                    \1 NVRVD => 8 CXFTF\n\
                                    \1 VJHF, 6 MNCFX => 4 RFSQX\n\
                                    \176 ORE => 6 VJHF"

example5_oreFor1Fuel = 2210736
example5_fuelForTrillionOre = 460664
example5 :: [Reaction]
example5 = parse' parser "example5" "171 ORE => 8 CNZTR\n\
                                    \7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
                                    \114 ORE => 4 BHXH\n\
                                    \14 VRPVC => 6 BMBT\n\
                                    \6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
                                    \6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
                                    \15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
                                    \13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
                                    \5 BMBT => 4 WPTQ\n\
                                    \189 ORE => 9 KTJDG\n\
                                    \1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
                                    \12 VRPVC, 27 CNZTR => 2 XDBXC\n\
                                    \15 KTJDG, 12 BHXH => 5 XCVML\n\
                                    \3 BHXH, 2 VRPVC => 7 MZWV\n\
                                    \121 ORE => 7 VRPVC\n\
                                    \7 XCVML => 6 RJRHP\n\
                                    \5 BHXH, 4 VRPVC => 5 LTCX"

simpleCollection = IngredientCollection (M.singleton "a" 1)

day14Tests = testGroup "day 14"
  [ testCase "part 1" $ part1 >>= (@?= 431448)
  , testGroup "remove ingredient"
    [ testCase "subtracts" $
        removeIngredient (Ingredient "a" 1) (IngredientCollection (M.singleton "a" 1)) @?= Just mempty
    , testCase "won't go below 0" $
        removeIngredient (Ingredient "a" 2) (mkIngredientCollection [Ingredient "a" 1]) @?= Nothing
    ]
  , testGroup "add ingredient"
    [ testCase "add 1 to empty collection gives 1" $
        addIngredient (Ingredient "a" 1) mempty @?= mkIngredientCollection [Ingredient "a" 1]
    , testCase "add 1 to collection with 3 already gives 4" $
        addIngredient (Ingredient "a" 1) (mkIngredientCollection [Ingredient "a" 3]) @?= mkIngredientCollection [Ingredient "a" 4]
    ]
  , testGroup "reactions"
    [ testCase "reverse" $
        let reaction = Reaction (mkIngredientCollection [Ingredient "a" 2, Ingredient "b" 10]) (Ingredient "c" 2)
        in reverseReaction reaction (mkIngredientCollection [Ingredient "c" 3]) @?= Just (mkIngredientCollection [Ingredient "a" 2, Ingredient "b" 10, Ingredient "c" 1])
    , testCase "forward" $
        let reaction = Reaction (mkIngredientCollection [Ingredient "a" 2, Ingredient "b" 10]) (Ingredient "c" 2)
            start = mkIngredientCollection [Ingredient "a" 2, Ingredient "b" 11, Ingredient "c" 10]
        in performReaction reaction start @?= Just (mkIngredientCollection [Ingredient "b" 1, Ingredient "c" 12])

    , testCase "reverse' with extra" $
        let reaction = Reaction (mkIngredientCollection [Ingredient "a" 1]) (Ingredient "c" 2)
        in reverseReaction' reaction (mkIngredientCollection [Ingredient "c" 1]) @?= mkIngredientCollection [Ingredient "a" 1, Ingredient "c" (-1)]
    , testCase "reverse' with 2 inputesextra" $
        let reaction = Reaction (mkIngredientCollection [Ingredient "a" 1, Ingredient "b" 2]) (Ingredient "c" 2)
        in reverseReaction' reaction (mkIngredientCollection [Ingredient "c" 1]) @?= mkIngredientCollection [Ingredient "a" 1, Ingredient "b" 2, Ingredient "c" (-1)]
    ]
  ]

mkIngredient :: (String, Integer) -> Ingredient
mkIngredient (s,i) = Ingredient s i

-- too low: 2320000

-- 998
part2_1000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",2000),("BJVQM",2036),("BLPJK",2000),("BMJR",1014),("CTVK",1009),("CWBNX",34),("DGFG",4010),("DZFNW",5994),("FDSX",29),("FHFH",2005),("FJRFG",5014),("FUEL",998),("GSCG",2002),("HPKB",1020),("JFKGV",7022),("JHDW",4),("JHXBM",8),("JLRM",7008),("JNKH",5),("JQNF",5098),("KDFNZ",12),("KGSDJ",5004),("KQDVM",21),("KZFPJ",6000),("LKPQG",4),("LMNCH",4002),("MGCQ",4001),("MPKR",2002),("MQXF",2002),("MZKG",4022),("NPRST",7025),("NWDZ",1060),("ORE",999568552000),("PQZXH",2007),("PVTL",3040),("QMBM",48),("QVNKN",3005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",4012),("SXKT",1074),("VJMWM",9),("VJNBT",2002),("VLWQB",10),("VQDT",3013),("VRQR",1036),("WBQR",2),("WHVXC",5003),("WJDF",5008),("XLFZJ",5998),("XQBS",2),("XSNKB",3025),("ZJGH",3034),("ZKGMX",2006),("ZKTX",1037),("ZSCX",3000)]

-- 1998
part2_2000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",4000),("BJVQM",4036),("BLPJK",4000),("BMJR",2014),("CTVK",2009),("CWBNX",34),("DGFG",8010),("DZFNW",11994),("FDSX",29),("FHFH",4005),("FJRFG",10014),("FUEL",1998),("GSCG",4002),("HPKB",2020),("JFKGV",14022),("JHDW",4),("JHXBM",8),("JLRM",14008),("JNKH",5),("JQNF",10098),("KDFNZ",12),("KGSDJ",10004),("KQDVM",21),("KZFPJ",12000),("LKPQG",4),("LMNCH",8002),("MGCQ",8001),("MPKR",4002),("MQXF",4002),("MZKG",8022),("NPRST",14025),("NWDZ",2060),("ORE",999137104000),("PQZXH",4007),("PVTL",6040),("QMBM",48),("QVNKN",6005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",8012),("SXKT",2074),("VJMWM",9),("VJNBT",4002),("VLWQB",10),("VQDT",6013),("VRQR",2036),("WBQR",2),("WHVXC",10003),("WJDF",10008),("XLFZJ",11998),("XQBS",2),("XSNKB",6025),("ZJGH",6034),("ZKGMX",4006),("ZKTX",2037),("ZSCX",6000)]

-- 2998
part2_3000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",6000),("BJVQM",6036),("BLPJK",6000),("BMJR",3014),("CTVK",3009),("CWBNX",34),("DGFG",12010),("DZFNW",17994),("FDSX",29),("FHFH",6005),("FJRFG",15014),("FUEL",2998),("GSCG",6002),("HPKB",3020),("JFKGV",21022),("JHDW",4),("JHXBM",8),("JLRM",21008),("JNKH",5),("JQNF",15098),("KDFNZ",12),("KGSDJ",15004),("KQDVM",21),("KZFPJ",18000),("LKPQG",4),("LMNCH",12002),("MGCQ",12001),("MPKR",6002),("MQXF",6002),("MZKG",12022),("NPRST",21025),("NWDZ",3060),("ORE",998705656000),("PQZXH",6007),("PVTL",9040),("QMBM",48),("QVNKN",9005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",12012),("SXKT",3074),("VJMWM",9),("VJNBT",6002),("VLWQB",10),("VQDT",9013),("VRQR",3036),("WBQR",2),("WHVXC",15003),("WJDF",15008),("XLFZJ",17998),("XQBS",2),("XSNKB",9025),("ZJGH",9034),("ZKGMX",6006),("ZKTX",3037),("ZSCX",9000)]

-- 3998
part2_4000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",8000),("BJVQM",8036),("BLPJK",8000),("BMJR",4014),("CTVK",4009),("CWBNX",34),("DGFG",16010),("DZFNW",23994),("FDSX",29),("FHFH",8005),("FJRFG",20014),("FUEL",3998),("GSCG",8002),("HPKB",4020),("JFKGV",28022),("JHDW",4),("JHXBM",8),("JLRM",28008),("JNKH",5),("JQNF",20098),("KDFNZ",12),("KGSDJ",20004),("KQDVM",21),("KZFPJ",24000),("LKPQG",4),("LMNCH",16002),("MGCQ",16001),("MPKR",8002),("MQXF",8002),("MZKG",16022),("NPRST",28025),("NWDZ",4060),("ORE",998274208000),("PQZXH",8007),("PVTL",12040),("QMBM",48),("QVNKN",12005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",16012),("SXKT",4074),("VJMWM",9),("VJNBT",8002),("VLWQB",10),("VQDT",12013),("VRQR",4036),("WBQR",2),("WHVXC",20003),("WJDF",20008),("XLFZJ",23998),("XQBS",2),("XSNKB",12025),("ZJGH",12034),("ZKGMX",8006),("ZKTX",4037),("ZSCX",12000)]

-- 4998
part2_5000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",10000),("BJVQM",10036),("BLPJK",10000),("BMJR",5014),("CTVK",5009),("CWBNX",34),("DGFG",20010),("DZFNW",29994),("FDSX",29),("FHFH",10005),("FJRFG",25014),("FUEL",4998),("GSCG",10002),("HPKB",5020),("JFKGV",35022),("JHDW",4),("JHXBM",8),("JLRM",35008),("JNKH",5),("JQNF",25098),("KDFNZ",12),("KGSDJ",25004),("KQDVM",21),("KZFPJ",30000),("LKPQG",4),("LMNCH",20002),("MGCQ",20001),("MPKR",10002),("MQXF",10002),("MZKG",20022),("NPRST",35025),("NWDZ",5060),("ORE",997842760000),("PQZXH",10007),("PVTL",15040),("QMBM",48),("QVNKN",15005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",20012),("SXKT",5074),("VJMWM",9),("VJNBT",10002),("VLWQB",10),("VQDT",15013),("VRQR",5036),("WBQR",2),("WHVXC",25003),("WJDF",25008),("XLFZJ",29998),("XQBS",2),("XSNKB",15025),("ZJGH",15034),("ZKGMX",10006),("ZKTX",5037),("ZSCX",15000)]

-- 9998
part2_10000 = mkIngredientCollection . fmap mkIngredient $ [("BCNV",20000),("BJVQM",20036),("BLPJK",20000),("BMJR",10014),("CTVK",10009),("CWBNX",34),("DGFG",40010),("DZFNW",59994),("FDSX",29),("FHFH",20005),("FJRFG",50014),("FUEL",9998),("GSCG",20002),("HPKB",10020),("JFKGV",70022),("JHDW",4),("JHXBM",8),("JLRM",70008),("JNKH",5),("JQNF",50098),("KDFNZ",12),("KGSDJ",50004),("KQDVM",21),("KZFPJ",60000),("LKPQG",4),("LMNCH",40002),("MGCQ",40001),("MPKR",20002),("MQXF",20002),("MZKG",40022),("NPRST",70025),("NWDZ",10060),("ORE",995685520000),("PQZXH",20007),("PVTL",30040),("QMBM",48),("QVNKN",30005),("QWKRZ",15),("RBXB",20),("RFLX",4),("RPWKC",40012),("SXKT",10074),("VJMWM",9),("VJNBT",20002),("VLWQB",10),("VQDT",30013),("VRQR",10036),("WBQR",2),("WHVXC",50003),("WJDF",50008),("XLFZJ",59998),("XQBS",2),("XSNKB",30025),("ZJGH",30034),("ZKGMX",20006),("ZKTX",10037),("ZSCX",30000)]