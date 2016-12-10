import java.security.MessageDigest

class Day5(inputPrefix: String) {

  type Md5Hash = String

  def hashesStartingWithFiveZerosFromPrefixedInput: Iterator[Md5Hash] = {

    // each hex digit takes 4 bits, so 5 take 20 bits, or 2.5 bytes
    // binary of 11110000 (0xF0), will bitwise & to 0 with anything that has the first 4 bits set to 0
    def first5HexAre0(arrayOfBytes: Array[Byte]): Boolean =
      arrayOfBytes(0) == 0 && arrayOfBytes(1) == 0 && (arrayOfBytes(2) & 0xF0) == 0

    Iterator.from(0)
      .map(i => MessageDigest.getInstance("MD5")
                             .digest((inputPrefix + i).getBytes))
      .filter(first5HexAre0)
      .map(_.map("%02X".format(_))
            .mkString)
  }

  private def getPart1Char(md5Hash: Md5Hash): Char = md5Hash.charAt(5)

  def getPart1Password: String = {
    hashesStartingWithFiveZerosFromPrefixedInput
      .take(8)
      .map(getPart1Char)
      .mkString
  }

  private def getPart2Index(md5Hash: Md5Hash): Int = md5Hash.charAt(5).asDigit
  private def getPart2Char(md5Hash: Md5Hash): Char = md5Hash.charAt(6)

  def getPart2Password: String = {
    val emtpyArray = Array.fill[Option[Char]](8)(None)
    hashesStartingWithFiveZerosFromPrefixedInput.scanLeft(emtpyArray) { (array, m) =>
      val index = getPart2Index(m)
      val hashMatchesEmptyPosition = index < array.length && array(index).isEmpty
      if (hashMatchesEmptyPosition) array(index) = Some(getPart2Char(m))
      array
    }
    .filter(_.forall(_.isDefined))
    .next
    .map(_.get)
    .mkString
  }

  //   inefficient, but seems easier to follow
  //  def getPart2Password: String = {
  //    (0 until 8).map { index =>
  //      hashesFromPrefixedInput
  //        .filter(getPart2Index(_) == index)
  //        .next()
  //    }.map(getPart2Char)
  //      .mkString
  //  }
}
