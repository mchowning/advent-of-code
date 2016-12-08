import org.scalatest.{Matchers, FreeSpec}

class Day4Spec extends FreeSpec with Matchers {

  "Day4" - {
    "can parse name" in {
      Day4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]").encryptedName shouldBe "aaaaa-bbb-z-y-x"
    }
  }



}
