import java.io.InputStream

import org.scalatest.{Matchers, FreeSpec}
import util.PipeOps._

class Day2Spec extends FreeSpec with Matchers {

  "Day2" - {
    "handles non edge moves" in {
      Day2.getPart1CoordinateFromSingleMove((1,1), 'U') shouldBe (1,0)
      Day2.getPart1CoordinateFromSingleMove((1,1), 'R') shouldBe (2,1)
      Day2.getPart1CoordinateFromSingleMove((1,1), 'D') shouldBe (1,2)
      Day2.getPart1CoordinateFromSingleMove((1,1), 'L') shouldBe (0,1)
    }
    "handles edge moves" in {
      Day2.getPart1CoordinateFromSingleMove((0,0), 'L') shouldBe (0,0)
      Day2.getPart1CoordinateFromSingleMove((0,0), 'U') shouldBe (0,0)
      Day2.getPart1CoordinateFromSingleMove((1,0), 'U') shouldBe (1,0)
      Day2.getPart1CoordinateFromSingleMove((2,0), 'U') shouldBe (2,0)
      Day2.getPart1CoordinateFromSingleMove((2,0), 'R') shouldBe (2,0)

      Day2.getPart1CoordinateFromSingleMove((0,1), 'L') shouldBe (0,1)
      Day2.getPart1CoordinateFromSingleMove((2,1), 'R') shouldBe (2,1)

      Day2.getPart1CoordinateFromSingleMove((0,2), 'L') shouldBe (0,2)
      Day2.getPart1CoordinateFromSingleMove((0,2), 'D') shouldBe (0,2)
      Day2.getPart1CoordinateFromSingleMove((1,2), 'D') shouldBe (1,2)
      Day2.getPart1CoordinateFromSingleMove((2,2), 'D') shouldBe (2,2)
      Day2.getPart1CoordinateFromSingleMove((2,2), 'R') shouldBe (2,2)
    }
    "handles multiple moves" in {
      Day2.getPart1CoordinateFromMultipleMoves((1,1), "ULL") shouldBe (0,0)
      Day2.getPart1CoordinateFromMultipleMoves((0,0), "RRDDD") shouldBe (2,2)
      Day2.getPart1CoordinateFromMultipleMoves((2,2), "LURDL") shouldBe (1,2)
      Day2.getPart1CoordinateFromMultipleMoves((1,2), "UUUUD") shouldBe (1,1)
    }
    "converts position to digit" in {
      Day2.getPart1Digit((0,0)) shouldBe 1
      Day2.getPart1Digit((1,0)) shouldBe 2
      Day2.getPart1Digit((2,0)) shouldBe 3
      Day2.getPart1Digit((0,1)) shouldBe 4
      Day2.getPart1Digit((1,1)) shouldBe 5
      Day2.getPart1Digit((2,1)) shouldBe 6
      Day2.getPart1Digit((0,2)) shouldBe 7
      Day2.getPart1Digit((1,2)) shouldBe 8
      Day2.getPart1Digit((2,2)) shouldBe 9
    }
    "determines valid coordinates for part 1" in {
      Day2.isValidPart1Coord((0, -1)) shouldBe false
      Day2.isValidPart1Coord((1, -1)) shouldBe false
      Day2.isValidPart1Coord((2, -1)) shouldBe false
      Day2.isValidPart1Coord((-1,0))  shouldBe false
      Day2.isValidPart1Coord((3,0))   shouldBe false
      Day2.isValidPart1Coord((-1,1))  shouldBe false
      Day2.isValidPart1Coord((3,1))   shouldBe false
      Day2.isValidPart1Coord((-1,2))  shouldBe false
      Day2.isValidPart1Coord((3,2))   shouldBe false
      Day2.isValidPart1Coord((0,3))   shouldBe false
      Day2.isValidPart1Coord((1,3))   shouldBe false
      Day2.isValidPart1Coord((2,3))   shouldBe false

      Day2.isValidPart1Coord((0,0)) shouldBe true
      Day2.isValidPart1Coord((1,0)) shouldBe true
      Day2.isValidPart1Coord((2,0)) shouldBe true
      Day2.isValidPart1Coord((0,1)) shouldBe true
      Day2.isValidPart1Coord((1,1)) shouldBe true
      Day2.isValidPart1Coord((2,1)) shouldBe true
      Day2.isValidPart1Coord((0,1)) shouldBe true
      Day2.isValidPart1Coord((1,1)) shouldBe true
      Day2.isValidPart1Coord((2,1)) shouldBe true
    }
    "determines valid coordinates for part 2" in {

      Day2.isValidPart2Coord((0,0))   shouldBe false
      Day2.isValidPart2Coord((1,0))   shouldBe false
      Day2.isValidPart2Coord((2,0))   shouldBe true
      Day2.isValidPart2Coord((3,0))   shouldBe false
      Day2.isValidPart2Coord((4,0))   shouldBe false
      Day2.isValidPart2Coord((2, -1)) shouldBe false

      Day2.isValidPart2Coord((-1,1))  shouldBe false
      Day2.isValidPart2Coord((0,1))   shouldBe false
      Day2.isValidPart2Coord((1,1))   shouldBe true
      Day2.isValidPart2Coord((2,1))   shouldBe true
      Day2.isValidPart2Coord((3,1))   shouldBe true
      Day2.isValidPart2Coord(4, 1)    shouldBe false
      Day2.isValidPart2Coord(5, 1)    shouldBe false

      Day2.isValidPart2Coord((2, -1)) shouldBe false
      Day2.isValidPart2Coord((2,0))   shouldBe true
      Day2.isValidPart2Coord((2,1))   shouldBe true
      Day2.isValidPart2Coord((2,2))   shouldBe true
      Day2.isValidPart2Coord((2,3))   shouldBe true
      Day2.isValidPart2Coord((2,4))   shouldBe true
      Day2.isValidPart2Coord((2,5))   shouldBe false

      Day2.isValidPart2Coord((3, -1)) shouldBe false
      Day2.isValidPart2Coord((3,0))   shouldBe false
      Day2.isValidPart2Coord((3,1))   shouldBe true
      Day2.isValidPart2Coord((3,2))   shouldBe true
      Day2.isValidPart2Coord((3,3))   shouldBe true
      Day2.isValidPart2Coord((3,4))   shouldBe false
      Day2.isValidPart2Coord((3,5))   shouldBe false

      Day2.isValidPart2Coord((4,0))   shouldBe false
      Day2.isValidPart2Coord((4,1))   shouldBe false
      Day2.isValidPart2Coord((4,2))   shouldBe true
      Day2.isValidPart2Coord((4,3))   shouldBe false
      Day2.isValidPart2Coord((4,4))   shouldBe false
      Day2.isValidPart2Coord((5,2))   shouldBe false
    }

    "from a series of moves" - {
      val seriesOfMoves = List("ULL", "RRDDD", "LURDL", "UUUUD")
      "gets multiple coordinates" in {
        Day2.getPart1Coordinates(seriesOfMoves) shouldBe List((0, 0),
                                                              (2, 2),
                                                              (1, 2),
                                                              (1, 1))
      }
      "gets multiple digits" in {
        Day2.getPart1Digits(seriesOfMoves) shouldBe "1985"
      }
      "handles part 2 keyboard layout" in {
        Day2.getPart2Keys(seriesOfMoves) shouldBe "5DB3"
      }
    }

    "solves" - {
      val stream: InputStream = getClass.getResourceAsStream("input_day2.txt")
      val input = io.Source.fromInputStream(stream).mkString
      val inputList = input.lines.toList
      "part 1" in {
        Day2.getPart1Digits(inputList) shouldBe "65556"
      }
      "part 2" in {
        Day2.getPart2Keys(inputList) shouldBe "CB779"
      }
    }
    "alternative solves" - {
      val input = (getClass.getResourceAsStream("input_day2.txt") |>
                    io.Source.fromInputStream).getLines.toList
      "part 1" in {
        val actual = input |> Day2Alternative.parseInstructions(Day2Alternative.Phone.Five)
        actual shouldBe List(Day2Alternative.Phone.Six,
                             Day2Alternative.Phone.Five,
                             Day2Alternative.Phone.Five,
                             Day2Alternative.Phone.Five,
                             Day2Alternative.Phone.Six)
      }
      "part 2" in {
        val actual = input |> Day2Alternative.parseInstructions(Day2Alternative.Potty.Five)
        actual shouldBe List(Day2Alternative.Potty.C,
                             Day2Alternative.Potty.B,
                             Day2Alternative.Potty.Seven,
                             Day2Alternative.Potty.Seven,
                             Day2Alternative.Potty.Nine)
      }
    }
  }
}
