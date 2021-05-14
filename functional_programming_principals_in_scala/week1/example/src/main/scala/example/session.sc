object session {

  def sum(xs: List[Int]): Int = if (xs.isEmpty) 0 else xs.head + sum(xs.tail)

  def max(xs: List[Int]): Int = {
    def getMax(x: Int, y: Int): Int = if (x >= y) x else y
    if (xs.isEmpty) Int.MinValue else getMax(xs.head, max(xs.tail))
  }

}

import session.sum
import session.max

sum(List(1, 2, 3, 4))
sum(List(-1, 2, -3, 4))

max(List(1, 2, 3, 4))
max(List(1, 2, -3, -4))