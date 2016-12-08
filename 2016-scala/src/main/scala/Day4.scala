import scala.util.parsing.combinator.RegexParsers

case class Room(encryptedName: String, sectorId: Int, checksum: String)

object RoomParser extends RegexParsers {

  def name: Parser[String] = """[a-z\-]+""".r ^^ { _.toString.init }
  def number: Parser[Int] = """[0-9]*""".r ^^ {_.toInt }
  def checksum: Parser[String] = "[" ~> """\w*""".r <~ "]" ^^ { _.toString }

  def full: Parser[Room] = name ~ number ~ checksum ^^ {
    case name ~ number ~ checksum => new Room(name, number, checksum)
  }
}

object Day4 extends RegexParsers {

  def parseRoom(s: String): Room = {
    RoomParser.parse(RoomParser.full, s).get
  }

  def getExpectedChecksum(room: Room): String = {

    val fullChecksum = room.encryptedName
                               .groupBy(identity)
                               .values
                               .filter(_.head != '-')
                               .toList
                               .groupBy(_.length)
                               .toSeq
                               .sortBy(_._1)
                               .reverse
                               .flatMap(_._2.sorted.map(_.head))
                               .mkString

    fullChecksum.substring(0, Math.min(fullChecksum.length, 5))
  }

  def hasValidChecksum(room: Room): Boolean = room.checksum == getExpectedChecksum(room)

  def getRoomsWithValidChecksums(strings: List[String]): List[Room] = strings.map(parseRoom)
                                                                        .filter(hasValidChecksum)
}
