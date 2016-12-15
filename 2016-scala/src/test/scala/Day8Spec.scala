import org.scalatest.{Matchers, FreeSpec}

class Day8Spec extends FreeSpec with Matchers {

  "with an empty starting display" - {
    val column = List.fill(3)(false)
    val input: List[List[Boolean]] = List.fill(3)(column)
    "can turn on" - {
      val withMatrix = Day8.turnOn(input) _
      "the top left pixel" in {
        withMatrix(0, 0) shouldBe List(List(true, false, false),
                                       List.fill(3)(false),
                                       List.fill(3)(false))
      }
      "the bottom left pixel" in {
        withMatrix(2, 0) shouldBe List(List.fill(3)(false),
                                       List.fill(3)(false),
                                       List(true, false, false))
      }
      "the bottom right pixel" in {
        withMatrix(2, 2) shouldBe List(List.fill(3)(false),
                                       List.fill(3)(false),
                                       List(false, false, true))
      }
      "the top right pixel" in {
        withMatrix(0, 2) shouldBe List(List(false, false, true),
                                       List.fill(3)(false),
                                       List.fill(3)(false))
      }
    }
    "can handle rect command" - {
      "that is tall" in {
        Day8.rect(input)(1, 2) shouldBe List(List(true, false, false),
                                             List(true, false, false),
                                             List.fill(3)(false))
      }
      "that is wide" in {
        Day8.rect(input)(2, 1) shouldBe List(List(true, true, false),
                                             List.fill(3)(false),
                                             List.fill(3)(false))
      }
      "from text" in {
        Day8.parse(input)("rect 2x2") shouldBe List(List(true, true, false),
                                                    List(true, true, false),
                                                    List.fill(3)(false))
      }
    }
    "can rotate" - {
      val with00on = Day8.turnOn(input)(0, 0)
      "row" - {
        "from parsed input" in {
          val from00 = Day8.rotateRow(with00on) _
          from00(0, 1) shouldBe List(List(false, true, false),
                                     List.fill(3)(false),
                                     List.fill(3)(false))
          from00(0, 2) shouldBe List(List(false, false, true),
                                     List.fill(3)(false),
                                     List.fill(3)(false))
          from00(0, 3) shouldBe List(List(true, false, false),
                                     List.fill(3)(false),
                                     List.fill(3)(false))
        }
        "from text" in {
          Day8.parse(with00on)("rotate column x=0 by 2") shouldBe List(List.fill(3)(false),
                                                                       List.fill(3)(false),
                                                                       List(true, false, false))
        }
      }
      "col" - {
        "from parsed input" in {
          val from00 = Day8.rotateCol(with00on) _
          from00(0, 1) shouldBe List(List.fill(3)(false),
                                     List(true, false, false),
                                     List.fill(3)(false))
          from00(0, 2) shouldBe List(List.fill(3)(false),
                                     List.fill(3)(false),
                                     List(true, false, false))
          from00(0, 3) shouldBe List(List(true, false, false),
                                     List.fill(3)(false),
                                     List.fill(3)(false))
        }
        "from text" in {
          Day8.parse(with00on)("rotate row y=0 by 1") shouldBe List(List(false, true, false),
                                                                    List.fill(3)(false),
                                                                    List.fill(3)(false))
        }
      }
    }
    "follows instructions" - {
      "with rect 1x1" in {
        val emptyMatrix = List.fill(3)(List.fill(3)(false))
        Day8.followInstructions(emptyMatrix)(List("rect 1x1")) shouldBe List(List(true, false, false),
                                                                             List.fill(3)(false),
                                                                             List.fill(3)(false))
      }
      "with rect 1x1, and rotate row y=0 by 1" in {
        val emptyMatrix = List.fill(3)(List.fill(3)(false))
        Day8.followInstructions(emptyMatrix)(List("rect 1x1",
                                                  "rotate row y=0 by 1")) shouldBe List(List(false, true, false),
                                                                                        List.fill(3)(false),
                                                                                        List.fill(3)(false))
      }
      "with rect 1x1, rotate row y=0 by 1, and rotate column x=1 by 2" in {
        val emptyMatrix = List.fill(3)(List.fill(3)(false))
        Day8.followInstructions(emptyMatrix)(List("rect 1x1",
                                                  "rotate row y=0 by 1",
                                                  "rotate column x=1 by 2")) shouldBe List(List.fill(3)(false),
                                                                                           List.fill(3)(false),
                                                                                           List(false, true, false))
      }
    }
    "solves part 1" in {
      val testMatrix = List.fill(6)(List.fill(50)(false)) // 6 tall, 50 wide
      val testInstructions = TestUtils.getLines("input_day8.txt")
      val numActivated = Day8.followInstructions(testMatrix)(testInstructions)
        .map(_.count(_ == true))
        .sum
      numActivated shouldBe 123
    }
  }
}
