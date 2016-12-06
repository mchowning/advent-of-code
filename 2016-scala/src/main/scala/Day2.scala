
object Day2 {

  type Coordinate = (Int, Int)

  def move(coordinate: Coordinate, direction: Char): Coordinate = {
    val (x,y) = coordinate
    direction match {
      case 'U' => (x    , y + 1)
      case 'R' => (x + 1, y    )
      case 'D' => (x    , y - 1)
      case 'L' => (x - 1, y    )
    }
  }
}
