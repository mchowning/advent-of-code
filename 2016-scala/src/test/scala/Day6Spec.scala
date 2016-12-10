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
    }
  }
}
