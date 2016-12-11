
object Day7 {

  def supportsTLS(input: String): Boolean = {
    val Array(preBracket, bracketed, _) =  input.split("[\\[\\]]")
    isAbba(preBracket) && !isAbba(bracketed)
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
