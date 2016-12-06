import java.io.InputStream

import Day1._
import org.scalatest.FunSuite

class Day1Spec extends FunSuite {

  test("parse right move") {
    assert(Day1.parseMove("R3") == (RightTurn, 3))
  }

  test("parse left move") {
    assert(Day1.parseMove("L2") == (LeftTurn, 2))
  }

  test("throws excpetion if turn invalid") {
    assertThrows[Exception](Day1.parseMove("x2"))
  }

  test("parses multiple moves") {
    assert(Day1.parseMoves("R1, L3, R2") == List((RightTurn, 1), (LeftTurn, 3), (RightTurn, 2)))
  }

  test("updates direction based on turn") {
     assert(Day1.updateDirection(North, RightTurn) == East)
     assert(Day1.updateDirection(North, LeftTurn) == West)
     assert(Day1.updateDirection(East, RightTurn) == South)
     assert(Day1.updateDirection(East, LeftTurn) == North)
     assert(Day1.updateDirection(South, RightTurn) == West)
     assert(Day1.updateDirection(South, LeftTurn) == East)
     assert(Day1.updateDirection(West, RightTurn) == North)
     assert(Day1.updateDirection(West, LeftTurn) == South)
   }

  test("updates initial position based on turns") {
    def getDirection(pos: Position): Direction = pos match { case (p, _, _) => p }
    assert(getDirection(Day1.getFinalPosition("R1")) == East)
    assert(getDirection(Day1.getFinalPosition("L1")) == West)
    assert(getDirection(Day1.getFinalPosition("L1, R1, R1, R1")) == South)
  }

  test("R2, L3 gives (North, 2, 3) final position") {
    assert(Day1.getFinalPosition("R2, L3") == (North, 2, 3))
  }

  test("R2, R2, R2 gives (West, 0, -2) final position") {
    assert(Day1.getFinalPosition("R2, R2, R2") == (West, 0, -2))
  }

  test("R5, L5, R5, R3 gives (South, 10, 2) final position") {
    assert(Day1.getFinalPosition("R5, L5, R5, R3") == (South, 10, 2))
  }

  test("R2, L3 gives final position distance of 5") {
    assert(Day1.getFinalPositionDistance("R2, L3") == 5)
  }

  test("R2, R2, R2 gives final position distance of 2") {
    assert(Day1.getFinalPositionDistance("R2, R2, R2") == 2)
  }

  test("R5, L5, R5, R3 gives final position distance of 12") {
     assert(Day1.getFinalPositionDistance("R5, L5, R5, R3") == 12)
  }

  test("actual test input gives final position distance of ___") {
    val stream: InputStream = getClass.getResourceAsStream("input_day1")
    val input = io.Source.fromInputStream(stream).mkString
    assert(Day1.getFinalPositionDistance(input) == 307)
  }
}
