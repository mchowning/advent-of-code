import org.scalatest.{FreeSpec, Matchers}

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
    "parses line of input (part 1)" in {
      Day3.parseTriangleFromLine("  810  679   10   ") shouldBe(810, 679, 10)
    }
    "parses triangles in columns (part 2)" in {
      Day3.parseTrianglesInColumns(List("  810 124 42  ", "   14 141  423 ", "  432 233 523"))
    }
    "counts number valid" in {
      val list = List("  1 1 1  ", "  2 2 3", "3 1 5  ", "1 3 5", "3 5 1")
      Day3.numValidPart1(list) shouldBe 2
    }
    "solves" - {
      val inputList = TestUtils.getLines("input_day3.txt")
      "part 1" in {
        Day3.numValidPart1(inputList) shouldBe 869
      }
      "solves part 2" in {
        Day3.numValidPart2(inputList) shouldBe 1544
      }
    }
  }
}
