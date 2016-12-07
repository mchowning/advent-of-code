import org.scalatest.{Matchers, FreeSpec}

class Day3Spec extends FreeSpec with Matchers {

  "Day3" - {
    "determines valid triangles" in {
      Day3.isValidTriangle((1,1,1)) shouldBe true
      Day3.isValidTriangle((2,2,3))
    }
    "determines invalid triangles" in {
      Day3.isValidTriangle((3,1,5)) shouldBe false
      Day3.isValidTriangle((1,3,5)) shouldBe false
      Day3.isValidTriangle((3,5,1)) shouldBe false
    }
    "parses line of input" in {
      Day3.parseTriangle("  810  679   10   ") shouldBe (810, 679, 10)
    }
  }
}
