
object Day7 {

  def isTlsSupported(input: String): Boolean = {
    val (unBracketed, bracketed) = separateBracketed(input)
    unBracketed.exists(isAbba) && !bracketed.exists(isAbba)
  }

  def separateBracketed(input: String): (List[String], List[String]) = {
    input.split("[\\[\\]]")
      .zipWithIndex
      .foldRight(List[String](), List[String]()) { (indexed, acc) =>
        val (unBracketed, bracketed) = acc
        val (letters, index) = indexed
        if (index % 2 == 0) (letters :: unBracketed, bracketed)
        else (unBracketed, letters :: bracketed)
      }
  }

  def isAbba(input: String): Boolean = {
    input.sliding(4)
      .exists(abbaCheck)
  }

  private def abbaCheck(input: String): Boolean = {
    assert(input.length == 4)
    val (front, end) = input.splitAt(2)
    front == end.reverse && front != end
  }

  def getAba(input: String): List[String] = {
    input
      .sliding(3)
      .filter { s => s.charAt(0) == s.charAt(2) && s.charAt(0) != s.charAt(1) }
      //.filter(_.toList match {
      //          case c0 :: c1 :: c2 :: Nil => c0 == c2 && c0 != c1
      //          case _ => throw new UnknownError // should never hit, but kills the warning
      //        })
      .toList
  }

  def isSslSupported(input: String): Boolean = {
    val (unBracketed, bracketed) = separateBracketed(input)
    unBracketed
      .flatMap(getAba)
      .exists(containsBabOf(bracketed, _))
  }

  private def containsBabOf(ls: List[String], aba: String): Boolean = {
    ls.flatMap(getAba)
      .exists(bab => aba(0) == bab(1) && bab(0) == aba(1))
  }
}
