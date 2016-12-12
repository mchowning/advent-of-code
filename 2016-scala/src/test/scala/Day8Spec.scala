import org.scalatest.{Matchers, FreeSpec}

class Day8Spec extends FreeSpec with Matchers {

  "with an empty starting display" - {
    val column = List.fill(3)(false)
    val input: List[List[Boolean]] = List.fill(3)(column)
    val subject = new Day8(input)
    "can turn on" - {
      "the top left pixel" in {
        val result = subject.turnOn(0, 0)
        result shouldBe List(List(true, false, false),
                             List.fill(3)(false),
                             List.fill(3)(false))
      }
      "the bottom right pixel" in {
        val result = subject.turnOn(2, 2)
        result shouldBe List(List.fill(3)(false),
                             List.fill(3)(false),
                             List(false, false, true))
      }
    }
  }
}
