import org.scalatest.FreeSpec

class Day2Spec extends FreeSpec {

  "Day2" - {
    "handles non edge moves" in {
      assert(Day2.move((1,1), 'U') == (1,2))
      assert(Day2.move((1,1), 'R') == (2,1))
      assert(Day2.move((1,1), 'D') == (1,0))
      assert(Day2.move((1,1), 'L') == (0,1))
    }
    "handles edge moves" in {
      assert(Day2.move((0,0), 'L') == (0,0))
      assert(Day2.move((0,0), 'D') == (0,0))
      assert(Day2.move((1,0), 'D') == (1,0))
      assert(Day2.move((2,0), 'D') == (2,0))
      assert(Day2.move((2,0), 'R') == (2,0))

      assert(Day2.move((0,1), 'L') == (0,1))
      assert(Day2.move((2,1), 'R') == (2,1))

      assert(Day2.move((0,2), 'L') == (0,2))
      assert(Day2.move((0,2), 'U') == (0,2))
      assert(Day2.move((1,2), 'U') == (1,2))
      assert(Day2.move((2,2), 'U') == (2,2))
      assert(Day2.move((2,2), 'R') == (2,2))
    }
    "handles multiple moves" in {
      assert(Day2.move((1,1), "ULL") == (0,2))
      assert(Day2.move((0,2), "RRDDD") == (2,0))
      assert(Day2.move((2,0), "LURDL") == (1,0))
      assert(Day2.move((1,0), "UUUUD") == (1,1))
    }
  }
}
