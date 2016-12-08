import org.scalatest.{Matchers, FreeSpec}

class Day4Spec extends FreeSpec with Matchers {

  "Day4 can" - {
    "parse Room" in {
      val parsedRoom = Day4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")
      parsedRoom.encryptedName shouldBe "aaaaa-bbb-z-y-x"
      parsedRoom.sectorId shouldBe 123
    }
  }



}
