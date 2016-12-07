

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

  def moveToCoordinate(coordinate: Coordinate,
                       direction: Char,
                       isValidCoord: Coordinate => Boolean): Coordinate = {

    val (x,y) = coordinate
    val newCoordinate = direction match {
      case 'U' => (x              , y - 1)
      case 'R' => (x + 1, y              )
      case 'D' => (x              , y + 1)
      case 'L' => (x - 1, y              )
    }
    if (isValidCoord(newCoordinate)) newCoordinate else coordinate
  }

  def moveToPart1Coordinate(coordinate: Coordinate, direction: Char): Coordinate =
    moveToCoordinate(coordinate, direction, validCoordPart1)

  def moveToPart2Coordinate(coordinate: Coordinate, direction: Char): Coordinate =
    moveToCoordinate(coordinate, direction, validCoordPart2)

  private def validCoordPart1(coordinate: Coordinate): Boolean = coordinate match {
    case (x,y) => x >= 0 &&
                  y >= 0 &&
                  x < part1KeypadSize &&
                  y < part1KeypadSize
  }

  def validCoordPart2(coordinate: Coordinate): Boolean = coordinate match {
    case (x,0) => x == 2
    case (x,1) => x > 0 && x < 4
    case (x,2) => x >= 0 && x <= 4
    case (x,3) => x > 0 && x < 4
    case (x,4) => x == 2
    case _     => false
  }

  def moveToPart1Coordinate(coordinate: Coordinate, directions: String): Coordinate = {
    (coordinate /: directions) { (coord, d) => moveToPart1Coordinate(coord, d) }
  }

  def moveToPart2Coordinate(coordinate: Coordinate, directions: String): Coordinate = {
    (coordinate /: directions) { (coord, d) => moveToPart2Coordinate(coord, d) } // TODO duplication
  }

  def getPart1Digit(coordinate: Coordinate): Int = {
    val (x,y) = coordinate
    (y * 3) + x + 1
  }

  private def getCoordinates(moves: List[String],
                             mover: (Coordinate, String) => Coordinate,
                             startCoordinate: Coordinate): List[Coordinate] =
    moves.scanLeft(startCoordinate)(mover)
         .tail

  def getPart1Coordinates(moves: List[String]): List[Coordinate] =
    getCoordinates(moves, moveToPart1Coordinate, (1,1))

  def getPart2Coordinates(moves: List[String]): List[Coordinate] =
    getCoordinates(moves, moveToPart2Coordinate, (0,2))

  def getPart1Digits(moves: List[String]): String = {
    getPart1Coordinates(moves)
    .map(getPart1Digit)
    .mkString
  }

  def getPart2Key(coordinate: Coordinate): Char = coordinate match {
    case (2,0) => '1'
    case (1,1) => '2'
    case (2,1) => '3'
    case (3,1) => '4'
    case (0,2) => '5'
    case (1,2) => '6'
    case (2,2) => '7'
    case (3,2) => '8'
    case (4,2) => '9'
    case (1,3) => 'A'
    case (2,3) => 'B'
    case (3,3) => 'C'
    case (2,4) => 'D'
  }

  def getPart2Keys(moves: List[String]): String = {
    getPart2Coordinates(moves)
      .map(getPart2Key)
      .mkString
  }
}
