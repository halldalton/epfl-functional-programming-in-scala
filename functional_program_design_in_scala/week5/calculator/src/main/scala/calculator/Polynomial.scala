package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def compute(): Set[Double] = {
      val delta  = computeDelta(a, b, c)()
      if (delta < 0) Set()
      else {
        val solution1  = (-b() + delta) / 2 * a()
        val solution2 = (-b() - delta) / 2 * a()
        if (solution1 != solution2) Set(solution1, solution2)
        else Set(solution1)
      }
    }
    Signal(compute())
  }
}
