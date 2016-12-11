import org.scalatest.{Matchers, FreeSpec}

class Day7Spec extends FreeSpec with Matchers {

  "Day 7" - {
    "checks for abba" - {
      "in simple 4-character cases" in {
        Day7.isAbba("abba") shouldBe true
        Day7.isAbba("abcd") shouldBe false
        Day7.isAbba("abcc") shouldBe false
      }
      "where letters just repeated" in {
        Day7.isAbba("aaaa") shouldBe false
      }
      "in longer sequences" in {
        Day7.isAbba("abbaasdjlfk") shouldBe true
        Day7.isAbba("asklfdabba") shouldBe true
        Day7.isAbba("askldjfabbaaljsfk") shouldBe true
      }
    }
    "checks IP for TLS support" - {
      "when there is no abba" in {
        Day7.supportsTLS("asdb[fjkl]jklas") shouldBe false
      }
    }
  }
}
