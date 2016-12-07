import org.scalatest.FreeSpec

class Day2Spec extends FreeSpec {

  "Day2" - {
    "handles non edge moves" in {
      assert(Day2.moveToCoordinate((1,1), 'U') ==(1,0))
      assert(Day2.moveToCoordinate((1,1), 'R') ==(2,1))
      assert(Day2.moveToCoordinate((1,1), 'D') ==(1,2))
      assert(Day2.moveToCoordinate((1,1), 'L') ==(0,1))
    }
    "handles edge moves" in {
      assert(Day2.moveToCoordinate((0,0), 'L') ==(0,0))
      assert(Day2.moveToCoordinate((0,0), 'U') ==(0,0))
      assert(Day2.moveToCoordinate((1,0), 'U') ==(1,0))
      assert(Day2.moveToCoordinate((2,0), 'U') ==(2,0))
      assert(Day2.moveToCoordinate((2,0), 'R') ==(2,0))

      assert(Day2.moveToCoordinate((0,1), 'L') ==(0,1))
      assert(Day2.moveToCoordinate((2,1), 'R') ==(2,1))

      assert(Day2.moveToCoordinate((0,2), 'L') ==(0,2))
      assert(Day2.moveToCoordinate((0,2), 'D') ==(0,2))
      assert(Day2.moveToCoordinate((1,2), 'D') ==(1,2))
      assert(Day2.moveToCoordinate((2,2), 'D') ==(2,2))
      assert(Day2.moveToCoordinate((2,2), 'R') ==(2,2))
    }
    "handles multiple moves" in {
      assert(Day2.moveToCoordinate((1,1), "ULL") ==(0,0))
      assert(Day2.moveToCoordinate((0,0), "RRDDD") ==(2,2))
      assert(Day2.moveToCoordinate((2,2), "LURDL") ==(1,2))
      assert(Day2.moveToCoordinate((1,2), "UUUUD") ==(1,1))
    }
    "converts position to digit" in {
      assert(Day2.getDigit((0,0)) == 1)
      assert(Day2.getDigit((1,0)) == 2)
      assert(Day2.getDigit((2,0)) == 3)
      assert(Day2.getDigit((0,1)) == 4)
      assert(Day2.getDigit((1,1)) == 5)
      assert(Day2.getDigit((2,1)) == 6)
      assert(Day2.getDigit((0,2)) == 7)
      assert(Day2.getDigit((1,2)) == 8)
      assert(Day2.getDigit((2,2)) == 9)
    }
    "from a series of moves" - {
      val seriesOfMoves = List("ULL", "RRDDD", "LURDL", "UUUUD")
      "gets multiple coordinates" in {
        assert(Day2.getCoordinates(seriesOfMoves) == List((0, 0),
                                                          (2, 2),
                                                          (1, 2),
                                                          (1, 1)))
      }
      "gets multiple digits" in {
        assert(Day2.getDigits(seriesOfMoves) == 1985)
      }
    }
  }
}
