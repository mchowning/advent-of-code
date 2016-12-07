import java.io.InputStream

import org.scalatest.FreeSpec

class Day2Spec extends FreeSpec {

  "Day2" - {
    "handles non edge moves" in {
      assert(Day2.getPart1CoordinateFromSingleMove((1,1), 'U') ==(1,0))
      assert(Day2.getPart1CoordinateFromSingleMove((1,1), 'R') ==(2,1))
      assert(Day2.getPart1CoordinateFromSingleMove((1,1), 'D') ==(1,2))
      assert(Day2.getPart1CoordinateFromSingleMove((1,1), 'L') ==(0,1))
    }
    "handles edge moves" in {
      assert(Day2.getPart1CoordinateFromSingleMove((0,0), 'L') ==(0,0))
      assert(Day2.getPart1CoordinateFromSingleMove((0,0), 'U') ==(0,0))
      assert(Day2.getPart1CoordinateFromSingleMove((1,0), 'U') ==(1,0))
      assert(Day2.getPart1CoordinateFromSingleMove((2,0), 'U') ==(2,0))
      assert(Day2.getPart1CoordinateFromSingleMove((2,0), 'R') ==(2,0))

      assert(Day2.getPart1CoordinateFromSingleMove((0,1), 'L') ==(0,1))
      assert(Day2.getPart1CoordinateFromSingleMove((2,1), 'R') ==(2,1))

      assert(Day2.getPart1CoordinateFromSingleMove((0,2), 'L') ==(0,2))
      assert(Day2.getPart1CoordinateFromSingleMove((0,2), 'D') ==(0,2))
      assert(Day2.getPart1CoordinateFromSingleMove((1,2), 'D') ==(1,2))
      assert(Day2.getPart1CoordinateFromSingleMove((2,2), 'D') ==(2,2))
      assert(Day2.getPart1CoordinateFromSingleMove((2,2), 'R') ==(2,2))
    }
    "handles multiple moves" in {
      assert(Day2.getPart1CoordinateFromMultipleMoves((1,1), "ULL") ==(0,0))
      assert(Day2.getPart1CoordinateFromMultipleMoves((0,0), "RRDDD") ==(2,2))
      assert(Day2.getPart1CoordinateFromMultipleMoves((2,2), "LURDL") ==(1,2))
      assert(Day2.getPart1CoordinateFromMultipleMoves((1,2), "UUUUD") ==(1,1))
    }
    "converts position to digit" in {
      assert(Day2.getPart1Digit((0,0)) == 1)
      assert(Day2.getPart1Digit((1,0)) == 2)
      assert(Day2.getPart1Digit((2,0)) == 3)
      assert(Day2.getPart1Digit((0,1)) == 4)
      assert(Day2.getPart1Digit((1,1)) == 5)
      assert(Day2.getPart1Digit((2,1)) == 6)
      assert(Day2.getPart1Digit((0,2)) == 7)
      assert(Day2.getPart1Digit((1,2)) == 8)
      assert(Day2.getPart1Digit((2,2)) == 9)
    }
    "determines valid coordinates for part 1" in {
      assert(!Day2.isValidPart1Coord((0, -1)))
      assert(!Day2.isValidPart1Coord((1, -1)))
      assert(!Day2.isValidPart1Coord((2, -1)))
      assert(!Day2.isValidPart1Coord((-1,0)))
      assert(!Day2.isValidPart1Coord((3,0)))
      assert(!Day2.isValidPart1Coord((-1,1)))
      assert(!Day2.isValidPart1Coord((3,1)))
      assert(!Day2.isValidPart1Coord((-1,2)))
      assert(!Day2.isValidPart1Coord((3,2)))
      assert(!Day2.isValidPart1Coord((0,3)))
      assert(!Day2.isValidPart1Coord((1,3)))
      assert(!Day2.isValidPart1Coord((2,3)))

      assert(Day2.isValidPart1Coord((0,0)))
      assert(Day2.isValidPart1Coord((1,0)))
      assert(Day2.isValidPart1Coord((2,0)))
      assert(Day2.isValidPart1Coord((0,1)))
      assert(Day2.isValidPart1Coord((1,1)))
      assert(Day2.isValidPart1Coord((2,1)))
      assert(Day2.isValidPart1Coord((0,1)))
      assert(Day2.isValidPart1Coord((1,1)))
      assert(Day2.isValidPart1Coord((2,1)))
    }
    "determines valid coordinates for part 2" in {

      assert(!Day2.isValidPart2Coord((0,0)))
      assert(!Day2.isValidPart2Coord((1,0)))
      assert(Day2.isValidPart2Coord((2,0)))
      assert(!Day2.isValidPart2Coord((3,0)))
      assert(!Day2.isValidPart2Coord((4,0)))
      assert(!Day2.isValidPart2Coord((2, -1)))

      assert(!Day2.isValidPart2Coord((-1,1)))
      assert(!Day2.isValidPart2Coord((0,1)))
      assert(Day2.isValidPart2Coord((1,1)))
      assert(Day2.isValidPart2Coord((2,1)))
      assert(Day2.isValidPart2Coord((3,1)))
      assert(!Day2.isValidPart2Coord(4, 1))
      assert(!Day2.isValidPart2Coord(5, 1))

      assert(!Day2.isValidPart2Coord((2, -1)))
      assert(Day2.isValidPart2Coord((2,0)))
      assert(Day2.isValidPart2Coord((2,1)))
      assert(Day2.isValidPart2Coord((2,2)))
      assert(Day2.isValidPart2Coord((2,3)))
      assert(Day2.isValidPart2Coord((2,4)))
      assert(!Day2.isValidPart2Coord((2,5)))

      assert(!Day2.isValidPart2Coord((3, -1)))
      assert(!Day2.isValidPart2Coord((3,0)))
      assert(Day2.isValidPart2Coord((3,1)))
      assert(Day2.isValidPart2Coord((3,2)))
      assert(Day2.isValidPart2Coord((3,3)))
      assert(!Day2.isValidPart2Coord((3,4)))
      assert(!Day2.isValidPart2Coord((3,5)))

      assert(!Day2.isValidPart2Coord((4,0)))
      assert(!Day2.isValidPart2Coord((4,1)))
      assert(Day2.isValidPart2Coord((4,2)))
      assert(!Day2.isValidPart2Coord((4,3)))
      assert(!Day2.isValidPart2Coord((4,4)))
      assert(!Day2.isValidPart2Coord((5,2)))
    }

    "from a series of moves" - {
      val seriesOfMoves = List("ULL", "RRDDD", "LURDL", "UUUUD")
      "gets multiple coordinates" in {
        assert(Day2.getPart1Coordinates(seriesOfMoves) == List((0, 0),
                                                               (2, 2),
                                                               (1, 2),
                                                               (1, 1)))
      }
      "gets multiple digits" in {
        assert(Day2.getPart1Digits(seriesOfMoves) == "1985")
      }
      "handles part 2 keyboard layout" in {
        assert(Day2.getPart2Keys(seriesOfMoves) == "5DB3")
      }
    }

    "solves" - {
      val stream: InputStream = getClass.getResourceAsStream("input_day2")
      val input = io.Source.fromInputStream(stream).mkString
      val inputList = input.lines.toList
      "part 1" in {
        assert(Day2.getPart1Digits(inputList) == "65556")
      }
      "part 2" in {
        assert(Day2.getPart2Keys(inputList) == "CB779")
      }
    }
  }
}
