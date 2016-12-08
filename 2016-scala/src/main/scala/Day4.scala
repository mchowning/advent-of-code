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
}
