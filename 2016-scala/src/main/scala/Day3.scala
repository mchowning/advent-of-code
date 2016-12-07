
object Day3 {

  type Triangle = (Int, Int, Int)

  def isValidTriangle(triangle: Triangle): Boolean = triangle match {
    case (a,b,c) => a + b > c &&
                    a + c > b &&
                    b + c > a
  }

  def parseTriangle(s: String): Triangle = {
    val array = s.split(' ')
                .filter(!_.isEmpty)
                .map(_.toInt)
    (array(0), array(1), array(2))
  }
}
