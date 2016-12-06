import java.io.InputStream

import Day1._
import org.scalatest.FreeSpec

class Day1Spec extends FreeSpec {

  "The Day1Spec" - {
    "can parse moves" - {
      "with a right turn" in {
        assert(Day1.parseMove("R3") == (RightTurn, 3))
      }
      "with a left turn" in {
        assert(Day1.parseMove("L2") == (LeftTurn, 2))
      }
      "in a list" in {
        assert(Day1.parseMoves("R1, L3, R2") == List((RightTurn, 1), (LeftTurn, 3), (RightTurn, 2)))
      }
      "that are invalid" in {
        assertThrows[Exception](Day1.parseMove("x2"))
      }
    }
    "can update a direction based on a turn" in {
      assert(Day1.updateDirection(North, RightTurn) == East)
      assert(Day1.updateDirection(North, LeftTurn) == West)
      assert(Day1.updateDirection(East, RightTurn) == South)
      assert(Day1.updateDirection(East, LeftTurn) == North)
      assert(Day1.updateDirection(South, RightTurn) == West)
      assert(Day1.updateDirection(South, LeftTurn) == East)
      assert(Day1.updateDirection(West, RightTurn) == North)
      assert(Day1.updateDirection(West, LeftTurn) == South)
    }
    "can update the direction of the initial position based on" - {
      def getDirection(pos: Position): Direction = pos match {case (p, _) => p}
      "a move with a right turn" in {
        assert(getDirection(Day1.getFinalPosition("R1")) == East)
      }
      "a move with a left turn" in {
        assert(getDirection(Day1.getFinalPosition("L1")) == West)
      }
      "a list of moves" in {
        assert(getDirection(Day1.getFinalPosition("L1, R1, R1, R1")) == South)
      }
    }
    "can determine the final position from a list of moves" in {
      assert(Day1.getFinalPosition("R2, L3") == (North, (2, 3)))
      assert(Day1.getFinalPosition("R2, R2, R2") == (West, (0, -2)))
      assert(Day1.getFinalPosition("R5, L5, R5, R3") == (South, (10, 2)))
    }
    "can determine the distance of the final position from a list of moves" in {
      assert(Day1.getFinalPositionDistance("R2, L3") == 5)
      assert(Day1.getFinalPositionDistance("R2, R2, R2") == 2)
      assert(Day1.getFinalPositionDistance("R5, L5, R5, R3") == 12)
    }
    "can solve part 1 of the problem" in {
      val stream: InputStream = getClass.getResourceAsStream("input_day1")
      val input = io.Source.fromInputStream(stream).mkString
      assert(Day1.getFinalPositionDistance(input) == 307)
    }
    "with a starting position" - {
      val startPosition = (North, (0, 0))
      "determine all positions visited during" - {
        "a move" in {
          assert(Day1.getVisitedPositions(startPosition, Day1.parseMove("R2")) == List((East, (1, 0)),
                                                                                       (East, (2, 0))))
          assert(Day1.getVisitedPositions(startPosition, Day1.parseMove("L1")) == List((West, (-1, 0))))
        }
        "a series of moves" in {
          assert(Day1.getVisitedPositions(startPosition, Day1.parseMoves("R2, L1")) ==
                 List((East, (1, 0)),
                      (East, (2, 0)),
                      (North, (2, 1))))
          assert(Day1.getVisitedPositions(startPosition, Day1.parseMoves("L2, L1")) ==
                 List((West, (-1, 0)),
                      (West, (-2, 0)),
                      (South, (-2, -1))))
        }
      }
      "determine the first position visited twice in a series of moves" in {
        val firstRepeatPosition = Day1.getFirstRepeatCoordinates(startingPosition,
                                                              Day1.parseMoves("R8, R4, R4, R8"))
        assert(firstRepeatPosition == (4, 0))
      }
      "determine the distance to the first position visited twice in a series of moves" in {
        val distanceToFirstRepeatPosition = Day1.getDistanceToFirstRepeatCoordinates("R8, R4, R4, R8")
        assert(distanceToFirstRepeatPosition == 4)
      }
    }
  }
}
