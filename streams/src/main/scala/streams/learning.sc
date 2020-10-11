import streams.{GameDef, Solver, StringParserTerrain}
trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
      /**
       * This method applies a list of moves `ls` to the block at position
       * `startPos`. This can be used to verify if a certain list of moves
       * is a valid solution, i.e. leads to the goal.
       */
      def solve(ls: List[Move]): Block =
            ls.foldLeft(startBlock) { case (block, move) =>
                  require(block.isLegal) // The solution must always lead to legal blocks
                  move match {
                        case Left => block.left
                        case Right => block.right
                        case Up => block.up
                        case Down => block.down
                  }
            }
}

trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

      val level =
            """ooo-------
              |oSoooo----
              |ooooooooo-
              |-ooooooooo
              |-----ooToo
              |------ooo-""".stripMargin

      val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
      val testBlock = Block(Pos(1,1),Pos(1,1))
      val testNWH = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
}


val l1 = new Level1{}
import l1._
val ls = l1.solution

ls
ls.foldLeft(startBlock) { case (block, move) => {
      println(block)
      println(move)
      require(block.isLegal, "nmsl block is not legal") // The solution must always lead to legal blocks
      move match {
            case Left => block.left
            case Right => block.right
            case Up => block.up
            case Down => block.down
      }
}
}





















































