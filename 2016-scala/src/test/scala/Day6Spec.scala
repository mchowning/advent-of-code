import org.scalatest.{Matchers, FreeSpec}

class Day6Spec extends FreeSpec with Matchers {

  "Day6" - {
    "for part 1" - {
      "gets the most common character from a list" in {
        Day6.getMostCommonChar("aba".toList) shouldBe 'a'
        Day6.getMostCommonChar("aab".toList) shouldBe 'a'
        Day6.getMostCommonChar("abbc".toList) shouldBe 'b'
        Day6.getMostCommonChar("abbcccdeeeeeeeee".toList) shouldBe 'e'
      }
      "gets most common letters by column" in {
        val input = List("eedadn",
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
        Day6.getPart1Message(input) shouldBe "easter"
      }
      "solves challenge" in {
        Day6.getPart1Message(TestUtils.getLines("input_day6.txt")) shouldBe "qzedlxso"
      }
    }
  }
}
