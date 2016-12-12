
object Day8 {

  type Matrix = List[List[Boolean]]

  def turnOn(m: Matrix)(x: Int, y: Int): Matrix = {
    val newColumnX = m(x).updated(y, true)
    m.updated(x, newColumnX)
  }

  def rect(m: Matrix)(wide: Int, tall: Int): Matrix = {
    val coords = for (x <- 0 until tall; y <- 0 until wide) yield (x,y)
    (m /: coords) { (o, cs) => turnOn(o)(cs._1, cs._2) }
  }
}
