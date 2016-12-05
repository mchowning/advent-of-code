
object Day1 {

  sealed trait Turn
  object RightTurn extends Turn
  object LeftTurn extends Turn

  sealed trait Direction
  object North extends Direction
  object East extends Direction
  object South extends Direction
  object West extends Direction

  type Move = (Turn, Int)

  def parseMoves(moves: String): List[Move] = {
    moves.split(",")
      .map(_.trim)
      .map(parseMove)
      .toList
  }

  def parseMove(move: String): Move = {
    val turn = move.head match {
      case 'R' => RightTurn
      case 'L' =>  LeftTurn
      case _   => throw new Exception("Move contains invalid Turn direction: should be 'R' or 'L'")
    }
    val distance = move.tail.toInt
    (turn, distance)
  }

  def updateDirection(direction: Direction, turn: Turn): Direction =  (direction, turn) match {
    case (North, RightTurn) => East
    case (North, LeftTurn)  => West
    case (East,  RightTurn) => South
    case (East,  LeftTurn)  => North
    case (South, RightTurn) => West
    case (South, LeftTurn)  => East
    case (West,  RightTurn) => North
    case (West,  LeftTurn)  => South
  }

}
