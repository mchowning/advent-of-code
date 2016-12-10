import org.scalatest.{Matchers, FreeSpec}

class Day6Spec extends FreeSpec with Matchers {

  val testInput = List("eedadn",
                       "drvtee",
                       "eandsr",
                       "raavrd",
                       "atevrs",
                       "tsrnev",
                       "sdttsa",
                       "rasrtv",
                       "nssdts",
                       "ntnada",
                       "svetve",
                       "tesnvt",
                       "vntsnd",
                       "vrdear",
                       "dvrsen",
                       "enarar")

  val realInput = TestUtils.getLines("input_day6.txt")

  "Day6" - {
    "for part 1" - {
      "gets the MOST common character from a list" in {
        Day6.getMostCommonChar("aba".toList) shouldBe 'a'
        Day6.getMostCommonChar("aab".toList) shouldBe 'a'
        Day6.getMostCommonChar("abbc".toList) shouldBe 'b'
        Day6.getMostCommonChar("abbcccdeeeeeeeee".toList) shouldBe 'e'
      }
      "gets most common letters by column" in {
        Day6.getPart1Message(testInput) shouldBe "easter"
      }
      "solves challenge" in {
        Day6.getPart1Message(realInput) shouldBe "qzedlxso"
      }
    }
    "for part 2" - {
      "gets the LEAST common character froma list" in {
        Day6.getLeastCommonChar("aba".toList) shouldBe 'b'
        Day6.getLeastCommonChar("aab".toList) shouldBe 'b'
        Day6.getLeastCommonChar("abbcc".toList) shouldBe 'a'
      }
      "gets least common letters by column" in {
        Day6.getPart2Message(testInput) shouldBe "advent"
      }
      "solves challenge" in {
        Day6.getPart2Message(realInput) shouldBe "ucmifjae"
      }
    }
  }
}
