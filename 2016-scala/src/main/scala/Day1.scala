object Day1 {

  sealed trait Turn
  object RightTurn extends Turn
  object LeftTurn extends Turn

  sealed trait Direction
  object North extends Direction
  object East extends Direction
  object South extends Direction
  object West extends Direction

  type Coordinates = (Int, Int)
  type Position = (Direction, (Coordinates))
  type Move = (Turn, Int)

  val startingPosition: Position = (North, (0, 0))

  def parseMoves(moves: String): List[Move] = {

    def parseSingleMove(move: String): Move = {
      val turn = move.head match {
        case 'R' => RightTurn
        case 'L' =>  LeftTurn
        case _   => throw new Exception("Move contains invalid Turn direction: should be 'R' or 'L'")
      }
      val distance = move.tail.toInt
      (turn, distance)
    }

    moves.split(",")
      .map(_.trim)
      .map(parseSingleMove)
      .toList
  }

  def updateDirection(direction: Direction, turn: Turn): Direction =
    (direction, turn) match {
      case (North, RightTurn) => East
      case (North, LeftTurn)  => West
      case (East,  RightTurn) => South
      case (East,  LeftTurn)  => North
      case (South, RightTurn) => West
      case (South, LeftTurn)  => East
      case (West,  RightTurn) => North
      case (West,  LeftTurn)  => South
    }

  def getFinalPosition(moves: List[Move]): Position = {
    (startingPosition /: moves) { (pos, move) =>
      val (direction, (x, y)) = pos
      val (turn, distance) = move
      val updatedDirection = updateDirection(direction, turn)
      getPosition(x, y, updatedDirection, distance)
    }
  }

  def getVisitedPositions(moves: List[Move]): List[Position] = {

    def getPositionsVisitedDuringSingleMove(startPos: Position, move: Move): List[Position] = {
      val (direction, (x, y)) = startPos
      val (turn, distance) = move
      val updatedDirection = updateDirection(direction, turn)
      for (i <- (1 to distance).toList) yield getPosition(x, y, updatedDirection, i)
    }

    (List(startingPosition) /: moves) { (posList, move) =>
      posList ::: getPositionsVisitedDuringSingleMove(posList.last, move)
    }.tail // drop starting position
  }

  private def getPosition(x: Int, y: Int, direction: Direction, distance: Int): Position =
    direction match {
      case North => (direction, (x           , y + distance))
      case South => (direction, (x           , y - distance))
      case East =>  (direction, (x + distance, y))
      case West =>  (direction, (x - distance, y))
   }

  def getFinalPositionDistance(moves: String): Int = {

    (parseMoves _
       andThen getFinalPosition
       andThen(_._2)
       andThen distanceFromStartTo)(moves)
  }

  def getFirstRepeatCoordinates(moves: List[Move]): (Int, Int) = {

    def getCoordinates(pos: Position): Coordinates = pos._2

    getVisitedPositions(moves)
      .map(getCoordinates)
      .inits
      .toList
      .reverse // order from shortest to longest
      .drop(2) // first two have no duplicates because they have length 0 and 1
      .filter { xs => xs.init.contains(xs.last) } // last element is the new element
      .head // first list with repeat
      .last // repeated element
  }

  def getDistanceToFirstRepeatCoordinates(moves: String): Int = {
    (parseMoves _
      andThen getFirstRepeatCoordinates
      andThen distanceFromStartTo)(moves)
  }

  private def distanceFromStartTo(coordinates: Coordinates): Int =  coordinates match {
    case (x, y) => Math.abs(x) + Math.abs(y)
  }
}
