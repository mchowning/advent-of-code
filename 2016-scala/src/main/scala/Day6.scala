
object Day6 {

  def getPart1Message(lines: List[String]): String = {
    lines.transpose
      .map(getMostCommonChar)
      .mkString
  }

  def getMostCommonChar: (List[Char]) => Char =
    _.groupBy(identity)
      .mapValues(_.size)
      .toList
      .sortBy(_._2)
      .last
      ._1
}
