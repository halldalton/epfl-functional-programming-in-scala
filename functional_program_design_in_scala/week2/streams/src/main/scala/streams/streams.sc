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

trait Level0 extends SolutionChecker {
  /* terrain for level 1*/

  val level =
    """oS-
      |oo-
      |oo-
      |To-""".stripMargin

  val optsolution = List(Down, Down)
}

val level = new Level0 {}

val neighbors = level.neighborsWithHistory(level.Block(level.Pos(1,1), level.Pos(2,1)), List(level.Down))
val newNeighbors = level.newNeighborsOnly(neighbors, Set(level.startBlock))

val init = level.Block(level.Pos(1, 1), level.Pos(1, 2))
val move = List(level.Down)
val hist = Set(level.startBlock)
val start = LazyList((init, move))

newNeighbors.toList

level.from(start, hist).toList