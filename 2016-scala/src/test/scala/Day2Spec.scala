import org.scalatest.FreeSpec

class Day2Spec extends FreeSpec {

  "Day2" - {
     "handles non edge moves" in {
       assert(Day2.move((1,1), 'U') == (1,2))
       assert(Day2.move((1,1), 'R') == (2,1))
       assert(Day2.move((1,1), 'D') == (1,0))
       assert(Day2.move((1,1), 'L') == (0,1))
     }
  }
}
