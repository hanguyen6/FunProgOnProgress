object WaterPouring {

  class Pouring(capacity: Vector[Int]) {

    // State
    type State = Vector[Int]
    val iniState = capacity map (x => 0)

    // Move
    trait Move {
      def change(state: State): State
    }

    case class Empty(glass: Int) extends Move {
      def change(state: State) = state updated(glass, 0)
    }

    case class Fill(glass: Int) extends Move {
      override def change(state: State): State = state updated(glass, capacity(glass))
    }
    case class Pour(from: Int, to: Int) extends Move {
      override def change(state: State): State = {
        val amount = state(from) min (capacity(to) - state(0))
        state updated(from, state(from) - amount) updated (to, state(to) + amount)
      }
    }

    // Set up
    val glasses = 0 until capacity.length
    val moves =
        (for (g <- glasses) yield Empty(g))  ++
        (for (g <- glasses) yield Fill(g)) ++
        (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))


    // Path
    class Path(history: List[Move]) {
      def endState: State = trackState(history)
      private def trackState(moves: List[Move]): State = moves match  {
        case Nil => iniState
        case move:: xs1 => move change trackState(xs1)
      }
    }


  }

  val problem = new Pouring(Vector(4,9))
  problem.moves
}