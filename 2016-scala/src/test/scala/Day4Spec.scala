import util.PipeOps._
import org.scalatest.{FreeSpec, Matchers}

class Day4Spec extends FreeSpec with Matchers {

  "Day4 can" - {
    "parse Room" in {
      val parsedRoom = RoomParser.parseString("aaaaa-bbb-z-y-x-123[abxyz]")
      parsedRoom.encryptedName shouldBe "aaaaa-bbb-z-y-x"
      parsedRoom.sectorId shouldBe 123
      parsedRoom.checksum shouldBe "abxyz"
    }
    "generates expected checksum" in {
      Day4.getExpectedChecksum(RoomParser.parseString("aaaaa-bbb-z-123[abc]"))       shouldBe "abz"
      Day4.getExpectedChecksum(RoomParser.parseString("a-bbb-zz-123[abc]"))          shouldBe "bza"
      Day4.getExpectedChecksum(RoomParser.parseString("z-bbb-aa-123[abc]"))          shouldBe "baz"
      Day4.getExpectedChecksum(RoomParser.parseString("aaaaa-bbb-z-y-x-123[abxyz]")) shouldBe "abxyz"
      Day4.getExpectedChecksum(RoomParser.parseString("a-b-c-d-e-f-g-h-987[abcde]")) shouldBe "abcde"
    }
    "validates checksum" in {
      Day4.hasValidChecksum(RoomParser.parseString("aaaaa-bbb-z-123[abz]"))         shouldBe true
      Day4.hasValidChecksum(RoomParser.parseString("a-bbb-zz-123[bza]"))            shouldBe true
      Day4.hasValidChecksum(RoomParser.parseString("z-bbb-aa-123[baz]"))            shouldBe true
      Day4.hasValidChecksum(RoomParser.parseString("aaaaa-bbb-z-y-x-123[abxyz]"))   shouldBe true
      Day4.hasValidChecksum(RoomParser.parseString("a-b-c-d-e-f-g-h-987[abcde]"))   shouldBe true

      Day4.hasValidChecksum(RoomParser.parseString("aaaaa-bbb-z-123[abzd]"))       shouldBe false
      Day4.hasValidChecksum(RoomParser.parseString("aaaaa-bbb-z-y-x-123[abyxz]"))  shouldBe false
      Day4.hasValidChecksum(RoomParser.parseString("a-b-c-d-e-f-g-h-987[abcdef]")) shouldBe false
    }
    "solves part 1" in {
      val rooms = TestUtils.getLines("input_day4.txt") |> Day4.getRoomsWithValidChecksums
      val expected = rooms.map(_.sectorId).sum
      expected shouldBe 361724
    }
    "shifts letter" in {
      Day4.shiftLetter(0)('a')   shouldBe 'a'
      Day4.shiftLetter(1)('a')   shouldBe 'b'
      Day4.shiftLetter(2)('a')   shouldBe 'c'
      Day4.shiftLetter(26)('a')  shouldBe 'a'
      Day4.shiftLetter(27)('a')  shouldBe 'b'
      Day4.shiftLetter(343)('q') shouldBe 'v'
      Day4.shiftLetter(343)('z') shouldBe 'e'
      Day4.shiftLetter(343)('-') shouldBe ' '
    }
    "decrypts name" in {
      RoomParser.parseString("qzmt-zixmtkozy-ivhz-343[aaaa]").decryptedName shouldBe "very encrypted name"
    }
    "solves part 2" in {
      val rooms = TestUtils.getLines("input_day4.txt") |> Day4.getRoomsWithValidChecksums
      val northPoleObjectStorage = rooms.filter(_.decryptedName == "northpole object storage")
                                        .head
      northPoleObjectStorage.sectorId shouldBe 482
    }
  }
}
