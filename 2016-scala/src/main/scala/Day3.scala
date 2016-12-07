import util.PipeOps._

object Day3 {

  type Triangle = (Int, Int, Int)

  def isValidTriangle(triangle: Triangle): Boolean = triangle match {
    case (a,b,c) => a + b > c &&
                    a + c > b &&
                    b + c > a
  }

  def parseLine(s: String): List[Int] = s.split(' ')
                                          .filter(!_.isEmpty)
                                          .map(_.toInt)
                                          .toList

  def parseTriangleFromList(array: List[Int]): Triangle = array match { case List(a,b,c) => (a,b,c) }

  def parseTriangleFromLine(s: String): Triangle = {
    s |> parseLine |> parseTriangleFromList
  }

  def parseTrianglesInColumns(lines: List[String]): List[Triangle] = {
    lines.map(parseLine)
     .grouped(3)
     .flatMap(_.transpose)
     .map(parseTriangleFromList)
     .toList
  }


  def numValidPart1(lines: List[String]): Int = {
    lines.map(parseTriangleFromLine)
      .count(isValidTriangle)
  }

  def numValidPart2(lines: List[String]): Int = {
    parseTrianglesInColumns(lines)
      .count(isValidTriangle)
  }
}
