import org.scalatest.{FreeSpec, Matchers}

class Day5Spec extends FreeSpec with Matchers {

  val testInputPrefix = "abbhdwsy"

  "Day5" - {
    "gets md5 hash" in {
      val subject = new Day5("")
      subject.md5("abc3231929") shouldBe "00000155F8105DFF7F56EE10FA9B9ABD"
      subject.md5("abc5017308") shouldBe "000008F82C5B3924A1ECBEBF60344E00"
      subject.md5("abc5278568") shouldBe "00000F9A2C309875E05C5A5D09F1B8C4"
    }
    "generates md5 hashes for successive inputs with base abc" in {
      new Day5(testInputPrefix)
        .hashesFromPrefixedInput
        .take(3)
        .toList shouldBe List("7E51386949E56DDAB4F31C503DE50F83",
                              "917B4F767F6713624AE0E4B4A4CD3CC9",
                              "1E2EC6125CC3E05CFD556134AE10E8AC")
    }
    "gets first 3 hashes with prefix 00" in {
      new Day5(testInputPrefix)
        .getFirstNHashesWithPrefix("00", 3) shouldBe List("00B98B3BA4DF35F1E8DEEA2E508638D9",
                                                          "004728CB6167C20592C4F9B681630C8C",
                                                          "0029840BEE223B0229715192ED9DDEF5")
    }
    "find subsequent hashes with prefix" - {
      val subject = new Day5("abc")
      "easy example" in {
        subject.getFirstNHashesWithPrefix("00", 1).head shouldBe "0034E0923CC38887A57BD7B1D4F953DF"
      }
      "tough examples" ignore {
        subject.getFirstNHashesWithPrefix("00000", 2) shouldBe List("00000155F8105DFF7F56EE10FA9B9ABD",
                                                                    "000008F82C5B3924A1ECBEBF60344E00")
      }
    }
    "solve part 1" ignore {
      new Day5(testInputPrefix).getPart1Password shouldBe "801B56A7"
    }
    "extracts part 2 password from list of hashes" in {
      new Day5("").getPart2Password(List("000003A",
                                         "000001R",
                                         "0000025aaa")) shouldBe "R5A"
    }
  }
}
