object curry {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

}

object exercise {

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factorial(n: Int):Int = product(x => x)(1, n)

}

curry.sum(x => x * x)(3, 5)
exercise.product(x => x * x)(3, 4)
exercise.factorial(5)