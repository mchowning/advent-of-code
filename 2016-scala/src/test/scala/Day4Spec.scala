import org.scalatest.{FreeSpec, Matchers}

class Day4Spec extends FreeSpec with Matchers {

  "Day4 can" - {
    "parse Room" in {
      val parsedRoom = Day4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")
      parsedRoom.encryptedName shouldBe "aaaaa-bbb-z-y-x"
      parsedRoom.sectorId shouldBe 123
      parsedRoom.checksum shouldBe "abxyz"
    }
    "generates expected checksum" in {
      Day4.getExpectedChecksum(Day4.parseRoom("aaaaa-bbb-z-123[abc]"))       shouldBe "abz"
      Day4.getExpectedChecksum(Day4.parseRoom("a-bbb-zz-123[abc]"))          shouldBe "bza"
      Day4.getExpectedChecksum(Day4.parseRoom("z-bbb-aa-123[abc]"))          shouldBe "baz"
      Day4.getExpectedChecksum(Day4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")) shouldBe "abxyz"
      Day4.getExpectedChecksum(Day4.parseRoom("a-b-c-d-e-f-g-h-987[abcde]")) shouldBe "abcde"
    }
    "validates checksum" in {
      Day4.hasValidChecksum(Day4.parseRoom("aaaaa-bbb-z-123[abz]"))         shouldBe true
      Day4.hasValidChecksum(Day4.parseRoom("a-bbb-zz-123[bza]"))            shouldBe true
      Day4.hasValidChecksum(Day4.parseRoom("z-bbb-aa-123[baz]"))            shouldBe true
      Day4.hasValidChecksum(Day4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]"))   shouldBe true
      Day4.hasValidChecksum(Day4.parseRoom("a-b-c-d-e-f-g-h-987[abcde]"))   shouldBe true

      Day4.hasValidChecksum(Day4.parseRoom("aaaaa-bbb-z-123[abzd]"))       shouldBe false
      Day4.hasValidChecksum(Day4.parseRoom("aaaaa-bbb-z-y-x-123[abyxz]"))  shouldBe false
      Day4.hasValidChecksum(Day4.parseRoom("a-b-c-d-e-f-g-h-987[abcdef]")) shouldBe false
    }
    "solves part 1" in {
      val expected = TestUtils.getLines("input_day4.txt")
                       .map(Day4.parseRoom)
                       .filter(Day4.hasValidChecksum)
                       .map(_.sectorId)
                       .sum
      expected shouldBe 361724
    }
  }
}
