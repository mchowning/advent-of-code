import java.io.InputStream

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
    "parses line of input" in {
      Day3.parseTriangle("  810  679   10   ") shouldBe (810, 679, 10)
    }
    "counts number valid" in {
      val list = List("  1 1 1  ", "  2 2 3", "3 1 5  ", "1 3 5", "3 5 1")
      Day3.numValid(list) shouldBe 2
    }
    "solves part 1" in {
      val stream: InputStream = getClass.getResourceAsStream("input_day3.txt")
      val input = io.Source.fromInputStream(stream).mkString
      val inputList = input.lines.toList
      Day3.numValid(inputList) shouldBe 869
    }
  }
}
