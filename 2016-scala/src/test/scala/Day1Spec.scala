import java.io.InputStream

import Day1._
import org.scalatest.{Matchers, FreeSpec}

class Day1Spec extends FreeSpec with Matchers {

  "Day1" - {
    "can parse moves" in {
      Day1.parseMoves("R1, L3, R2") shouldBe List((RightTurn, 1), (LeftTurn, 3), (RightTurn, 2))
    }
    "can update a direction based on a turn" in {
      Day1.updateDirection(North, RightTurn) shouldBe East
      Day1.updateDirection(North, LeftTurn)  shouldBe West
      Day1.updateDirection(East, RightTurn)  shouldBe South
      Day1.updateDirection(East, LeftTurn)   shouldBe North
      Day1.updateDirection(South, RightTurn) shouldBe West
      Day1.updateDirection(South, LeftTurn)  shouldBe East
      Day1.updateDirection(West, RightTurn)  shouldBe North
      Day1.updateDirection(West, LeftTurn)   shouldBe South
    }
    "can update the direction of the initial position based on" - {
      def getDirection(pos: Position): Direction = pos match {case (p, _) => p}
      "a move with a right turn" in {
        val moves: List[(Turn, Int)] = Day1.parseMoves("R1")
        getDirection(Day1.getFinalPosition(moves)) shouldBe East
      }
      "a move with a left turn" in {
        val moves: List[(Turn, Int)] = Day1.parseMoves("L1")
        getDirection(Day1.getFinalPosition(moves)) shouldBe West
      }
      "a list of moves" in {
        val moves: List[(Turn, Int)] = Day1.parseMoves("L1, R1, R1, R1")
        getDirection(Day1.getFinalPosition(moves)) shouldBe South
      }
    }
    "can determine the final position from a list of moves" in {

      val moves1: List[(Turn, Int)] = Day1.parseMoves("R2, L3")
      Day1.getFinalPosition(moves1) shouldBe (North, (2, 3))

      val moves2: List[(Turn, Int)] = Day1.parseMoves("R2, R2, R2")
      Day1.getFinalPosition(moves2) shouldBe (West, (0, -2))

      val moves: List[(Turn, Int)] = Day1.parseMoves("R5, L5, R5, R3")
      Day1.getFinalPosition(moves) shouldBe (South, (10, 2))
    }
    "can determine the distance of the final position from a list of moves" in {
      Day1.getFinalPositionDistance("R2, L3") shouldBe 5
      Day1.getFinalPositionDistance("R2, R2, R2") shouldBe 2
      Day1.getFinalPositionDistance("R5, L5, R5, R3") shouldBe 12
    }
    "can solve part 1 of the problem" in {
      val stream: InputStream = getClass.getResourceAsStream("input_day1.txt")
      val input = io.Source.fromInputStream(stream).mkString
      Day1.getFinalPositionDistance(input) shouldBe 307
    }
    "with a starting position determine all positions visited during a series of moves" in {
       Day1.getVisitedPositions(Day1.parseMoves("R2, L1")) shouldBe  List((East, (1, 0)),
                                                                          (East, (2, 0)),
                                                                          (North, (2, 1)))
       Day1.getVisitedPositions(Day1.parseMoves("L2, L1")) shouldBe List((West, (-1, 0)),
                                                                         (West, (-2, 0)),
                                                                         (South, (-2, -1)))
    }
    "can determine the first position visited twice in a series of moves" in {
      val firstRepeatPosition = Day1.getFirstRepeatCoordinates(Day1.parseMoves("R8, R4, R4, R8"))
      firstRepeatPosition shouldBe (4, 0)
    }
    "can determine the distance to the first position visited twice in a series of moves" in {
      val distanceToFirstRepeatPosition = Day1.getDistanceToFirstRepeatCoordinates("R8, R4, R4, R8")
      distanceToFirstRepeatPosition shouldBe 4
    }
    "can solve part 2" in {
      val stream: InputStream = getClass.getResourceAsStream("input_day1.txt")
      val input = io.Source.fromInputStream(stream).mkString
      getDistanceToFirstRepeatCoordinates(input) shouldBe 165
    }
  }
}
