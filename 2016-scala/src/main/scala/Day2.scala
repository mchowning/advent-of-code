
object Day2 {

  val keypadSize = 3

  // Coordinates represent the x,y coordinates on a keypad
  // with 0,0 as the bottom left key
  type Coordinate = (Int, Int)

  def move(coordinate: Coordinate, direction: Char): Coordinate = {

    def maxLimit(i: Int): Int = Math.min(i, keypadSize - 1)
    def minLimit(i: Int): Int = Math.max(i, 0)

    val (x,y) = coordinate
    direction match {
      case 'U' => (x              , maxLimit(y + 1))
      case 'R' => (maxLimit(x + 1), y              )
      case 'D' => (x              , minLimit(y - 1))
      case 'L' => (minLimit(x - 1), y              )
    }
  }

  def move(coordinate: Coordinate, directions: String): Coordinate = {
    (coordinate /: directions) { (coord, d) => move(coord, d) }
  }
}
