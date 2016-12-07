

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
  // with 0,0 as the top left position

  type Coordinate = (Int, Int)

  private def getCoordinateFromSingleMove(coordinate: Coordinate,
                                          direction: Char,
                                          isValidCoord: Coordinate => Boolean
                                         ): Coordinate = {
    val updatedCoordinate = {
      val (x, y) = coordinate
      direction match {
        case 'U' => (x, y - 1)
        case 'R' => (x + 1, y)
        case 'D' => (x, y + 1)
        case 'L' => (x - 1, y)
      }
    }

    if (isValidCoord(updatedCoordinate)) updatedCoordinate else coordinate
  }

  private def getCoordinateFromMultipleMoves(coordinate: Coordinate,
                                             directions: String,
                                             singleMoveCoordinateGetter: (Coordinate, Char) => Coordinate
                                            ): Coordinate = {
    (coordinate /: directions) { (coord, d) => singleMoveCoordinateGetter(coord, d) }
  }

  private def getCoordinates(moves: List[String],
                             mover: (Coordinate, String) => Coordinate,
                             startCoordinate: Coordinate): List[Coordinate] =
    moves.scanLeft(startCoordinate)(mover)
         .tail // drop start coordinate

  private def getKeys(moves: List[String],
                      coordGetter: List[String] => List[Coordinate],
                      keyNameGetter: Coordinate => String
                     ): String =
    coordGetter(moves)
      .map(keyNameGetter)
      .mkString

  /*
   * part 1
   */

  def getPart1Digits(moves: List[String]): String = {
    val digitGetter = getPart1Digit _ andThen (_.toString)
    getKeys(moves, getPart1Coordinates, digitGetter)
  }

  def getPart1Coordinates(moves: List[String]): List[Coordinate] =
    getCoordinates(moves, getPart1CoordinateFromMultipleMoves, (1,1))

  def getPart1CoordinateFromMultipleMoves(coordinate: Coordinate, directions: String): Coordinate = {
    getCoordinateFromMultipleMoves(coordinate, directions, getPart1CoordinateFromSingleMove)
  }

  def getPart1CoordinateFromSingleMove(coordinate: Coordinate, direction: Char): Coordinate =
    getCoordinateFromSingleMove(coordinate, direction, isValidPart1Coord)

  def getPart1Digit(coordinate: Coordinate): Int = {
    val (x,y) = coordinate
    (y * 3) + x + 1
  }

  def isValidPart1Coord(coordinate: Coordinate): Boolean = coordinate match {
    case (x,y) => x >= 0 &&
                  y >= 0 &&
                  x < part1KeypadSize &&
                  y < part1KeypadSize
  }

  /*
   * part 2
   */

  def getPart2Keys(moves: List[String]): String =
    getKeys(moves, getPart2Coordinates, getPart2Key)

  def getPart2Coordinates(moves: List[String]): List[Coordinate] =
    getCoordinates(moves, getPart2CoordinateFromMultipleMoves, (0,2))

  def getPart2CoordinateFromMultipleMoves(coordinate: Coordinate, directions: String): Coordinate =
    getCoordinateFromMultipleMoves(coordinate, directions, getPart2CoordinateFromSingleMove)

  def getPart2CoordinateFromSingleMove(coordinate: Coordinate, direction: Char): Coordinate =
    getCoordinateFromSingleMove(coordinate, direction, isValidPart2Coord)

  def isValidPart2Coord(coordinate: Coordinate): Boolean = coordinate match {
    case (x,0) => x == 2
    case (x,1) => x > 0 && x < 4
    case (x,2) => x >= 0 && x <= 4
    case (x,3) => x > 0 && x < 4
    case (x,4) => x == 2
    case _     => false
  }

  def getPart2Key(coordinate: Coordinate): String = coordinate match {
    case (2,0) => "1"
    case (1,1) => "2"
    case (2,1) => "3"
    case (3,1) => "4"
    case (0,2) => "5"
    case (1,2) => "6"
    case (2,2) => "7"
    case (3,2) => "8"
    case (4,2) => "9"
    case (1,3) => "A"
    case (2,3) => "B"
    case (3,3) => "C"
    case (2,4) => "D"
  }
}
