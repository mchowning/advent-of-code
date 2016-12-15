import scala.util.parsing.combinator.RegexParsers

trait Type
case class Rect(width: Int, height: Int) extends Type
case class RotateColumn(column: Int, amount: Int) extends Type
case class RotateRow(row: Int, amount: Int) extends Type

object Day8Parser extends RegexParsers {
  private def rect: Parser[String] = """rect""".r ^^ { _.toString }
  private def rotateColumn: Parser[String] = """rotate column x=""".r ^^ { _.toString }
  private def rotateRow: Parser[String] = """rotate row y=""".r ^^ { _.toString }
  private def digit: Parser[Int] = """\d+""".r ^^ { _.toInt }
  private def x: Parser[String] = """x""".r ^^ { _.toString }
  private def by: Parser[String] = """by""".r ^^ { _.toString }

  private def fullRect: Parser[Rect] = rect ~ digit ~ x ~ digit map {
    case _ ~ width ~ _ ~ height  => new Rect(width, height)
  }

  private def fullColumn: Parser[RotateColumn] = rotateColumn ~ digit ~ by ~ digit map {
    case _ ~ col ~ _ ~ amount => new RotateColumn(col, amount)
  }

  private def fullRow: Parser[RotateRow] = rotateRow ~ digit ~ by ~ digit map {
    case _ ~ row ~ _ ~ amount => new RotateRow(row, amount)
  }

  private def fullParser: Parser[Type] = fullRect | fullColumn | fullRow

  def parse(s: String): Type = parse(fullParser, s).get
}

object Day8 {

  type Matrix = List[List[Boolean]]

  def turnOn(m: Matrix)(row: Int, col: Int): Matrix = {
    val newRow = m(row).updated(col, true)
    m.updated(row, newRow)
  }

  def rect(m: Matrix)(wide: Int, height: Int): Matrix = {
    val coords = for (x <- 0 until height; y <- 0 until wide) yield (x,y)
    (m /: coords) { (o, cs) => turnOn(o)(cs._1, cs._2) }
  }
  
  def rotateRow(m: Matrix)(row: Int, amount: Int): Matrix = {

    def rotateRowOnePlace(m1: Matrix, row: Int) = {
      val oldRow = m1(row)
      val newRow = oldRow.last :: oldRow.init
      m.updated(row, newRow)
    }

    (m /: (0 until amount)) { (m1, _) => rotateRowOnePlace(m1, row)}
  }

  def rotateCol(m: Matrix)(col: Int, amount: Int): Matrix =
    rotateRow(m.transpose)(col, amount).transpose

  def parse(m: Matrix)(s: String): Matrix = {
    Day8Parser.parse(s) match {
      case Rect(width, height) => rect(m)(width, height)
      case RotateColumn(col, amount) => rotateCol(m)(col, amount)
      case RotateRow(row, amount) => rotateRow(m)(row, amount)
      case _ => throw new UnknownError("Parse failed to match case. This should never happen.")
    }
  }
}
