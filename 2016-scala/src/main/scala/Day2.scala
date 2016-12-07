
object Day2 {

  val keypadSize = 3

  // keypad is laid out:
  // 1 2 3
  // 4 5 6
  // 7 8 9

  // Coordinates represent the x,y coordinates on a keypad
  // with 0,0 as the top left key

  type Coordinate = (Int, Int)

  def move(coordinate: Coordinate, direction: Char): Coordinate = {

    def maxLimit(i: Int): Int = Math.min(i, keypadSize - 1)
    def minLimit(i: Int): Int = Math.max(i, 0)

    val (x,y) = coordinate
    direction match {
      case 'U' => (x              , minLimit(y - 1))
      case 'R' => (maxLimit(x + 1), y              )
      case 'D' => (x              , maxLimit(y + 1))
      case 'L' => (minLimit(x - 1), y              )
    }
  }

  def move(coordinate: Coordinate, directions: String): Coordinate = {
    (coordinate /: directions) { (coord, d) => move(coord, d) }
  }

  def getDigit(coordinate: Coordinate): Int = {
    val (x,y) = coordinate
    (y * 3) + x + 1
  }
}
