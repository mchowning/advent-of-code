

object Day2 {

  val part1KeypadSize = 3

  // part 1 keypad is laid out:
  // 1 2 3
  // 4 5 6
  // 7 8 9

  // part 2 keypad is laid out:
  //     1
  //   2 3 4
  // 5 6 7 8 9
  //   A B C
  //     D

  // Coordinates represent the x,y coordinates on a keypad
  // with 0,0 as the top left key

  type Coordinate = (Int, Int)

  def moveToPart1Coordinate(coordinate: Coordinate, direction: Char): Coordinate = {

    def validCoord(coordinate: Coordinate): Boolean = coordinate match {
      case (x,y) => x >= 0 &&
                      y >= 0 &&
                      x < part1KeypadSize &&
                      y < part1KeypadSize
    }

    val (x,y) = coordinate
    val newCoordinate = direction match {
      case 'U' => (x              , y - 1)
      case 'R' => (x + 1, y              )
      case 'D' => (x              , y + 1)
      case 'L' => (x - 1, y              )
    }
    if (validCoord(newCoordinate)) newCoordinate else coordinate
  }

  def moveToPart1Coordinate(coordinate: Coordinate, directions: String): Coordinate = {
    (coordinate /: directions) { (coord, d) => moveToPart1Coordinate(coord, d) }
  }

  def getPart1Digit(coordinate: Coordinate): Int = {
    val (x,y) = coordinate
    (y * 3) + x + 1
  }

  def getPart1Coordinates(moves: List[String]): List[Coordinate] = {
    moves.scanLeft((1,1))(moveToPart1Coordinate)
         .tail
  }

  def getDigits(moves: List[String]): String = {
    getPart1Coordinates(moves)
    .map(getPart1Digit)
    .mkString
  }
}
