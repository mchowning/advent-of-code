
object Day1 {

  sealed trait Turn
  object RightTurn extends Turn
  object LeftTurn extends Turn

  sealed trait Direction
  object North extends Direction
  object East extends Direction
  object South extends Direction
  object West extends Direction

  type Position = (Direction, Int, Int)
  type Move = (Turn, Int)

  val startingPosition: Position = (North, 0, 0)

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

  def getFinalPosition(moves: String): Position = {
    (startingPosition /: parseMoves(moves)) { (pos, move) =>
      val (direction, x, y) = pos
      val (turn, distance) = move
      val updatedDirection = updateDirection(direction, turn)
      getPosition(x, y, updatedDirection, distance)
    }
  }

  def getVisitedPositions(startPos: Position, move: Move): List[Position] = {
    val (direction, x, y) = startPos
    val (turn, distance) = move
    val updatedDirection = updateDirection(direction, turn)
    for (i <- (1 to distance).toList) yield getPosition(x, y, updatedDirection, i)
  }

  def getPosition(x: Int, y: Int, direction: Direction, distance: Int): Position = {
    direction match {
      case North => (direction, x           , y + distance)
      case South => (direction, x           , y - distance)
      case East =>  (direction, x + distance, y)
      case West =>  (direction, x - distance, y)
    }
  }

  def getFinalPositionDistance(moves: String): Int = {
    val (_, x, y) = getFinalPosition(moves)
    Math.abs(x) + Math.abs(y)
  }
}
