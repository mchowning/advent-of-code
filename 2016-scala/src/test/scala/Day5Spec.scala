import org.scalatest.{FreeSpec, Matchers}

class Day5Spec extends FreeSpec with Matchers {

  "Day5" - {
    "gets md5 hash" in {
      Day5.md5("abc3231929") shouldBe "00000155F8105DFF7F56EE10FA9B9ABD"
      Day5.md5("abc5017308") shouldBe "000008F82C5B3924A1ECBEBF60344E00"
      Day5.md5("abc5278568") shouldBe "00000F9A2C309875E05C5A5D09F1B8C4"
    }
    "generates md5 hashes for successive inputs with base abc" in {
      Day5.hashesFromPrefixedInput.take(3).toList shouldBe List("7E51386949E56DDAB4F31C503DE50F83",
                                                                "917B4F767F6713624AE0E4B4A4CD3CC9",
                                                                "1E2EC6125CC3E05CFD556134AE10E8AC")
    }
    "gets first 3 hashes with prefix 00" in {
      Day5.getFirstNHashesWithPrefix("00", 3) shouldBe List("00B98B3BA4DF35F1E8DEEA2E508638D9",
                                                            "004728CB6167C20592C4F9B681630C8C",
                                                            "0029840BEE223B0229715192ED9DDEF5")
    }
  }
}
