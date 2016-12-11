
object Day7 {

  def isAbba(input: String): Boolean = {
    val (front, end) = input.splitAt(2)
    front == end.reverse
  }
}
