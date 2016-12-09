import java.security.MessageDigest
import util.PipeOps._

class Day5(inputPrefix: String) {

  type Md5Hash = String

  def md5(s: String): Md5Hash = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map("%02X".format(_))
      .mkString
  }

  def hashesFromPrefixedInput: Iterator[Md5Hash] =
    Iterator.from(0)
      .map(inputPrefix + _ |> md5)

  def getFirstNHashesWithPrefix(hashPrefix: String, numToTake: Int): List[Md5Hash] =
    hashesFromPrefixedInput
      .filter(_.startsWith(hashPrefix))
      .take(numToTake)
      .toList

  def getPart1Password: String = {
    getFirstNHashesWithPrefix("00000", 8)
      .map(_.charAt(5))
      .mkString
  }

  def getPart2Password(hashes: List[Md5Hash]): String = {
    def processHash(hash: Md5Hash): (Int, Char) = {
      val index = hash.charAt(5).toInt
      val char = hash.charAt(6)
      (index, char)
    }

    hashes.map(processHash)
      .sortBy(_._1)
      .map(_._2)
      .mkString
  }
}
