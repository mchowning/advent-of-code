import scala.util.parsing.combinator.RegexParsers

case class Room(encryptedName: String, sectorId: Int, checksum: String)

object RoomParser extends RegexParsers {

  def name: Parser[String] = """[a-z\-]+""".r ^^ { _.toString.init }

  def full: Parser[Room] = name ^^ {
    case name => new Room(name, 0, "")
  }

}

object Day4 extends RegexParsers {

  def parseRoom(s: String): Room = {
    RoomParser.parse(RoomParser.full, s).get
  }
}
