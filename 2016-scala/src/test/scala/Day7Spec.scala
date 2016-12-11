import org.scalatest.{Matchers, FreeSpec}

class Day7Spec extends FreeSpec with Matchers {

  "Day 7" - {
    "checks for abba" - {
      "in simple 4-character cases" in {
        Day7.isAbba("abba") shouldBe true
        Day7.isAbba("dxxd") shouldBe true
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
      "when there is abba in initial section" - {
        "and nowhere else" in {
          Day7.supportsTLS("abba[fjkl]jklas") shouldBe true
          Day7.supportsTLS("laal[fjkl]jklas") shouldBe true
        }
      }
      "when there is abba in middle section" - {
        "and nowhere else" in {
          Day7.supportsTLS("lkja[rnnr]jklas") shouldBe false
        }
        "and in first section" in {
          Day7.supportsTLS("abba[nwwn]jklas") shouldBe false
        }
        "and in last section" in {
          Day7.supportsTLS("lkaj[ekke]laal") shouldBe false
        }
      }
    }
  }
}
