import scala.util.parsing.combinator.RegexParsers

case class Room(encryptedName: String, sectorId: Int, checksum: String) {

  def decryptedName: String = {
    encryptedName
      .map(Day4.shiftLetter(sectorId))
      .replace('-', ' ')
  }
}

object RoomParser extends RegexParsers {

  private def name: Parser[String] = """[a-z\-]+""".r ^^ { _.toString.init }
  private def number: Parser[Int] = """[0-9]*""".r ^^ {_.toInt }
  private def checksum: Parser[String] = "[" ~> """\w*""".r <~ "]" ^^ { _.toString }

  private def full: Parser[Room] = name ~ number ~ checksum ^^ {
    case name ~ number ~ checksum => new Room(name, number, checksum)
  }

  def parseString(s: String): Room = parse(full, s).get
}

object Day4 {

  def getExpectedChecksum(room: Room): String = {

    def characterFrequencies(s: String): Map[Int, Set[Char]] =
     s.groupBy(identity)
       .mapValues(_.length)
       .groupBy(_._2)
       .mapValues(_.keySet)

    def limitToChecksumLength(fullChecksum: String): String = {
      val maxChecksumLength = 5
      fullChecksum.substring(0, Math.min(fullChecksum.length,
                                         maxChecksumLength))
    }

    val charactersSortedByFrequencyThenLetter = characterFrequencies(room.encryptedName)
                                                 .toSeq
                                                 .sortBy(_._1)
                                                 .reverse
                                                 .flatMap(_._2.toSeq.sorted)
                                                 .filter(_ != '-')
                                                 .mkString

    limitToChecksumLength(charactersSortedByFrequencyThenLetter)
  }

  def hasValidChecksum(room: Room): Boolean = room.checksum == getExpectedChecksum(room)

  def getRoomsWithValidChecksums(strings: List[String]): List[Room] = strings.map(RoomParser.parseString)
                                                                        .filter(hasValidChecksum)

  def shiftLetter(num: Int)(c: Char): Char = {
    if (c == '-') ' ' else {
      ((c - 'a' + num) % 26 + 'a').toChar
    }
  }
}
