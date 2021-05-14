import scala.annotation.tailrec

object highOrder {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

}

highOrder.sum(x => x * x, 3, 5)