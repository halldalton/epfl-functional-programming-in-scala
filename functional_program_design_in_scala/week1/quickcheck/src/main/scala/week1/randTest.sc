import week1._

def test[T](r: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for (_ <- 0 until numTimes) {
    val value = r.generate
    assert(test(value), "Test failed for: " + value)
  }
  println("Test passed " + numTimes + " times")
}

test(Generators.pairs(Generators.lists, Generators.lists)) {
  case (xs, ys) => (xs ++ ys).length > xs.length
}