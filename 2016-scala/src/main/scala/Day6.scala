
object Day6 {

  def getPart1Message(lines: List[String]): String = {
    lines.transpose
      .map(getMostCommonChar)
      .mkString
  }

  def getMostCommonChar(ls: List[Char]): Char = getCharsSortedByFrequency(ls).last

  def getLeastCommonChar(ls: List[Char]): Char = getCharsSortedByFrequency(ls).head

  private def getCharsSortedByFrequency(ls: List[Char]): List[Char] = {
    ls.groupBy(identity)
      .mapValues(_.size)
      .toList
      .sortBy(_._2)
      .map(_._1)
  }

}
