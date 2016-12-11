import org.scalatest.{Matchers, FreeSpec}

class Day7Spec extends FreeSpec with Matchers {

  "Day 7" - {
    "determines simple 4-character ABBA cases" in {
      Day7.isAbba("abba") shouldBe true
      Day7.isAbba("abcd") shouldBe false
      Day7.isAbba("abcc") shouldBe false
    }
    "returns false if letters just repeated" in {
      Day7.isAbba("aaaa") shouldBe false
    }
  }
}
