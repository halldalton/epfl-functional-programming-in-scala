import week3._

import scala.annotation.tailrec

object find_nth {
  @tailrec
  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException()
    if (n == 0) xs.head
    else nth(n-1, xs.tail)
  }
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
find_nth.nth(2, list)