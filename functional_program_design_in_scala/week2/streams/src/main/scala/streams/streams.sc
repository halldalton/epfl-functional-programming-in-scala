case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
  if (levelVector.isEmpty) Map(Pos(0, 0) -> false) withDefaultValue false
  else {
    def isValidPos(c: Char): Boolean = if (c == '-') false else true
    (for {
      i <- levelVector.indices
      j <- levelVector(i).indices
    } yield Pos(i, j) -> isValidPos(levelVector(i)(j))).toMap withDefaultValue false
  }
}

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  val row = levelVector indexWhere (v => v contains c)
  val col = levelVector(row) indexOf c
  Pos(row, col)
}

val  level = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('z', 'o'), Vector('-', 'o'))

def function0: Pos => Boolean = terrainFunction(level)

function0(Pos(0, 1))
function0(Pos(3, 0))
function0(Pos(10, 10))

findChar('z', level)