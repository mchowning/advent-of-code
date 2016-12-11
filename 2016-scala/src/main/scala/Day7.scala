
object Day7 {

  def supportsTLS(input: String): Boolean = {

    val divided = input.split("[\\[\\]]")
      .zipWithIndex
      .foldRight(List[String](), List[String]()) {  (indexed, acc) =>
        val (everyOtherFromFirst, everyOtherFromSecond) = acc
        val (letters, index) = indexed
        if (index % 2 == 0) (letters :: everyOtherFromFirst, everyOtherFromSecond)
        else (everyOtherFromFirst, letters :: everyOtherFromSecond)
      }

    val (unBracketed, bracketed) = if (input.head == '[') divided.swap else divided
    unBracketed.exists(isAbba) && !bracketed.exists(isAbba)
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
}
