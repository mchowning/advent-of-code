
object Day8 {

  type Matrix = List[List[Boolean]]

  def turnOn(m: Matrix)(row: Int, col: Int): Matrix = {
    val newRow = m(row).updated(col, true)
    m.updated(row, newRow)
  }

  def rect(m: Matrix)(wide: Int, tall: Int): Matrix = {
    val coords = for (x <- 0 until tall; y <- 0 until wide) yield (x,y)
    (m /: coords) { (o, cs) => turnOn(o)(cs._1, cs._2) }
  }
  
  def rotateRow(m: Matrix)(row: Int, distance: Int): Matrix = {

    def rotateRowOnePlace(m1: Matrix, row: Int) = {
      val oldRow = m1(row)
      val newRow = oldRow.last :: oldRow.init
      m.updated(row, newRow)
    }

    (m /: (0 until distance)) { (m1,_) => rotateRowOnePlace(m1, row)}
  }

  def rotateCol(m: Matrix)(col: Int, distance: Int): Matrix =
    rotateRow(m.transpose)(col, distance).transpose
}
