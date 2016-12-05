
object Day1 {

  sealed trait Turn
  object RightTurn extends Turn
  object LeftTurn extends Turn

  type Move = (Turn, Int)

  def parseMove(move: String): Move = {
    val turn = move.head match {
      case 'R' => RightTurn
      case 'L' =>  LeftTurn
      case _   => throw new Exception("Move contains invalid Turn direction: should be 'R' or 'L'")
    }
    val distance = move.tail.toInt
    (turn, distance)
  }

}
