import org.scalatest.{Matchers, FreeSpec}

class Day8Spec extends FreeSpec with Matchers {

  "with an empty starting display" - {
    val column = List.fill(3)(false)
    val input: List[List[Boolean]] = List.fill(3)(column)
    "can turn on" - {
      val withMatrix = Day8.turnOn(input) _
      "the top left pixel" in {
        withMatrix(0, 0) shouldBe List(List(true, false, false),
                                       List.fill(3)(false),
                                       List.fill(3)(false))
      }
      "the bottom left pixel" in {
        withMatrix(2,0) shouldBe List(List.fill(3)(false),
                                      List.fill(3)(false),
                                      List(true, false, false))
      }
      "the bottom right pixel" in {
        withMatrix(2, 2) shouldBe List(List.fill(3)(false),
                                       List.fill(3)(false),
                                       List(false, false, true))
      }
      "the top right pixel" in {
        withMatrix(0,2) shouldBe List(List(false, false, true),
                                      List.fill(3)(false),
                                      List.fill(3)(false))
      }
    }
    "can handle rect command" - {
      "that is tall" in {
        Day8.rect(input)(1, 2) shouldBe List(List(true, false, false),
                                             List(true, false, false),
                                             List.fill(3)(false))
      }
      "that is wide" in {
        Day8.rect(input)(2, 1) shouldBe List(List(true, true, false),
                                             List.fill(3)(false),
                                             List.fill(3)(false))
      }
    }
  }
}
