import java.security.MessageDigest
import util.PipeOps._

object Day5 {

  def md5(s: String): String = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map("%02X".format(_))
      .mkString
  }

  def hashesFromPrefixedInput(inputPrefix: String): Iterator[String] =
    Iterator.from(0)
      .map(inputPrefix + _ |> md5)
}
