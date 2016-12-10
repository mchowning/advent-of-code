import org.scalatest.{FreeSpec, Matchers}

class Day5Spec extends FreeSpec with Matchers {

  val testInputPrefix = "abbhdwsy"

  "Day5" - {
    "solves part 1" in {
      new Day5(testInputPrefix).getPart1Password shouldBe "801B56A7"
    }
    "solves part 2" in {
      new Day5(testInputPrefix).getPart2Password shouldBe "424A0197"
    }
  }
}
