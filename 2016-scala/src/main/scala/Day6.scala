
object Day6 {

  def getPart1Message(lines: List[String]): String = getMessage(lines, getMostCommonChar)


  def getPart2Message(lines: List[String]): String = getMessage(lines, getLeastCommonChar)

  private def getMessage(lines: List[String], f: (List[Char] => Char)): String = {
    lines.transpose
      .map(f)
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
