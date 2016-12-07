import java.io.InputStream

object TestUtils {

  def getString(file: String) = {
    val stream: InputStream = getClass.getResourceAsStream(file)
    io.Source.fromInputStream(stream).mkString
  }

  def getLines(file: String): List[String] = {
    getString(file).lines.toList
  }
}
