import org.scalatest.{FreeSpec, Matchers}

class Day5Spec extends FreeSpec with Matchers {

  val inputPart1 = "abbhdwsy"

  "Day5" - {
    "gets md5 hash" in {
      Day5.md5("abc3231929") shouldBe "00000155F8105DFF7F56EE10FA9B9ABD"
      Day5.md5("abc5017308") shouldBe "000008F82C5B3924A1ECBEBF60344E00"
      Day5.md5("abc5278568") shouldBe "00000F9A2C309875E05C5A5D09F1B8C4"
    }
    "generates md5 hashes for successive inputs with base abc" in {
      Day5.hashesFromPrefixedInput("abc").take(3).toList shouldBe List("577571BE4DE9DCCE85A041BA0410F29F",
                                                                       "23734CD52AD4A4FB877D8A1E26E5DF5F",
                                                                       "63872B5565B2179BD72EA9C339192543")
    }
  }
}
