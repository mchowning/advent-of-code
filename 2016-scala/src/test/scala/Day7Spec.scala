import org.scalatest.{Matchers, FreeSpec}

class Day7Spec extends FreeSpec with Matchers {

  "Day 7" - {
    "for part 1" - {
      "checks for ABBA" - {
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
        "when there is abba in pre-brackets section and nowhere else" in {
          Day7.supportsTLS("abba[fjkl]jklas") shouldBe true
          Day7.supportsTLS("laal[fjkl]jklas") shouldBe true
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
        "when there is abba in post-brackets section and nowhere else" in {
          Day7.supportsTLS("asdf[asdfg]leel") shouldBe true
        }
        "when there are multiple brackets" in {
          Day7.supportsTLS("abba[sdlkfj]ajsl[abba]lkjl") shouldBe false
        }
      }
      "solves part 1" in {
        TestUtils.getLines("input_day7.txt").count(Day7.supportsTLS) shouldBe 110
      }
    }
    "for part 2" - {
      "finds all ABA" in {
        Day7.getAba("aba") shouldBe List("aba")
        Day7.getAba("abab") shouldBe List("aba", "bab")
        Day7.getAba("zzbzz") shouldBe List("zbz")
        Day7.getAba("zzbzzglg") shouldBe List("zbz", "glg")
      }
      "separates bracketed from unbracketed" in {
        val (unBracketed, bracketed) = Day7.separateBracketed("aaa[b]ccc[dddd]ee")
        unBracketed should contain only ("aaa", "ccc", "ee")
        bracketed should contain only ("b", "dddd")
      }
    }
  }
}
