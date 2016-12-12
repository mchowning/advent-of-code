
class Day8(matrix: List[List[Boolean]]) {

  type Matrix = List[List[Boolean]]

  def turnOn(x: Int, y: Int): Matrix = {
    val newColumnX = matrix(x).updated(y, true)
    matrix.updated(x, newColumnX)
  }
}
