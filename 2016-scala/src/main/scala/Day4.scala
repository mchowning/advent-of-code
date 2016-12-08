import scala.util.parsing.combinator.RegexParsers

case class Room(encryptedName: String, sectorId: Int, checksum: String)

object RoomParser extends RegexParsers {

  def name: Parser[String] = """[a-z\-]+""".r ^^ { _.toString.init }
  def number: Parser[Int] = """[0-9]*""".r ^^ {_.toInt }

  def full: Parser[Room] = name ~ number ^^ {
    case name ~ number => new Room(name, number, "")
  }

}

object Day4 extends RegexParsers {

  def parseRoom(s: String): Room = {
    RoomParser.parse(RoomParser.full, s).get
  }
}
