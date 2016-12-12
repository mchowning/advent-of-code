
object Day7 {

  def supportsTLS(input: String): Boolean = {
    val (unBracketed: List[String], bracketed: List[String]) = separateBracketed(input)
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
      .toList
  }
}
