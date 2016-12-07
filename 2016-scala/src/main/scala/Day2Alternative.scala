// Based on sleepynate's solution:
// https://github.com/sleepynate/adventofcode/blob/master/src/main/scala/com/sleepynate/adventofcode/Day2.scala

object Day2Alternative {

  trait Key {
    def up:Key = this
    def right:Key = this
    def down:Key = this
    def left:Key = this
  }

  case object Phone {
    case object One extends Key {
      override def right = Two
      override def down = Four
    }
    case object Two extends Key {
      override def right = Three
      override def down = Five
      override def left = One
    }
    case object Three extends Key {
      override def down = Six
      override def left = Two
    }
    case object Four extends Key {
      override def up = One
      override def right = Five
      override def down = Seven
    }
    case object Five extends Key {
      override def up = Two
      override def right = Six
      override def down = Eight
      override def left = Four
    }
    case object Six extends Key {
      override def up = Three
      override def down = Nine
      override def left = Five
    }
    case object Seven extends Key {
      override def up = Four
      override def right = Eight
    }
    case object Eight extends Key {
      override def up = Five
      override def right = Nine
      override def left = Seven
    }
    case object Nine extends Key {
      override def up = Six
      override def left = Eight
    }
  }

  case object Potty {
    case object One extends Key {
      override def down: Key = Three
    }
    case object Two extends Key {
      override def right: Key = Three
      override def down: Key = Six
    }
    case object Three extends Key {
      override def up: Key = One
      override def right: Key = Four
      override def down: Key = Seven
      override def left: Key = Two
    }
    case object Four extends Key {
      override def down: Key = Eight
      override def left: Key = Three
    }
    case object Five extends Key {
      override def right: Key = Six
    }
    case object Six extends Key {
      override def up: Key = Two
      override def right: Key = Seven
      override def down: Key = A
      override def left: Key = Five
    }
    case object Seven extends Key {
      override def up: Key = Three
      override def right: Key = Eight
      override def down: Key = B
      override def left: Key = Six
    }
    case object Eight extends Key {
      override def up: Key = Four
      override def right: Key = Nine
      override def down: Key = C
      override def left: Key = Seven
    }
    case object Nine extends Key {
      override def left: Key = Eight
    }
    case object A extends Key {
      override def up: Key = Six
      override def right: Key = B
    }
    case object B extends Key {
      override def up: Key = Seven
      override def right: Key = C
      override def down: Key = D
      override def left: Key = A
    }
    case object C extends Key {
      override def up: Key = Eight
      override def left: Key = B
    }
    case object D extends Key {
      override def up: Key = B
    }
  }

  def parseInstructions(start: Key)(listOfInstructionLines: List[String]) = {

    def keyMove(key: Key, c: Char) = c match {
      case 'U' => key.up
      case 'R' => key.right
      case 'D' => key.down
      case 'L' => key.left
    }

    def followLineOfInstructions(starting: Key, singleLineOfInstructions: String) =
      singleLineOfInstructions.foldLeft(starting)(keyMove)

    listOfInstructionLines.scanLeft(start)(followLineOfInstructions)
                          .tail // drop starting key
  }
}
