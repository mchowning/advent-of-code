import Day1Bench
import Day2Bench
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ day1,
      day2
    ]