import java.nio.file.Path

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
        val amount = state(from) min (capacity(to) - state(to))
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
    class Path(history: List[Move], val endState: State) {
      private def trackState(moves: List[Move]): State = moves match  {
        case Nil => iniState
        case move:: xs1 => move change trackState(xs1)
      }
      def extend(move: Move) = new Path (move :: history, move change endState)

      override def toString: String = (history.reverse mkString " ") + "--> " + endState
    }

    val iniPath = new Path(Nil, iniState)

    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves map path.extend
          if !(explored contains next.endState)
        } yield next
        paths #:: from(more, explored ++ (more.map(x => x.endState)))
      }

    val pathSets = from(Set(iniPath), Set(iniState))

    def solution (target: Int): Stream[Path] =
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState contains target
      } yield path

  }

  val problem = new Pouring(Vector(4,7))
  problem.moves
  problem.pathSets.take(3).toList.foreach(println)
  problem.solution(6)

}