import Day1.{LeftTurn, RightTurn}
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
}
