
class Pouring(capacity: Vector[Int]) {
  // States
  type State = Vector[Int]
  val initialState: State = capacity map {x => 0}

  trait Move {
    def change (state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      if (state(from) > (capacity(to) - state(to)) ) state.updated(to, capacity(to)).updated(from, state(from)-(capacity(to) - state(to)))
      else state.updated(from, 0).updated(to, state(to) + state(from))
    }
  }

  val glasses = 0 until capacity.length

  val moves = (for {g <- glasses} yield Empty(g)) ++
    (for {g <- glasses} yield Fill(g)) ++
    (for {f <- glasses
          t <- glasses
          if f != t} yield Pour(f, t))


  class Path(hist: List[Move]， val endState: State) {
    def extend(newMove: Move): Path = new Path(newMove :: hist, newMove change endState)
    override def toString: String = (hist.reverse mkString " ") + "--> " + endState.toString
  }

  val initPath = new Path(moves.toList, initialState)

  def  from(paths : Set[Path], visitedStates: Set[State]) : Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else  {
      val more = for {p <- paths;
                m <- moves map p.extend
                if  ! (visitedStates contains m.endState)
                      } yield m
      paths #:: from(more, Set.concat(visitedStates, more map (_.endState) ))
          }
  val pathSets = from(Set(initPath), Set(initialState))

  def solutions(target: Int): Stream[Path] = {
    for {pSet <- pathSets
        p <- pSet
         if p.endState contains target
         } yield p
    }

}
