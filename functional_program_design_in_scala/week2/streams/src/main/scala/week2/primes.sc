def from(n: Int): LazyList[Int] = n #:: from(n + 1)

val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 10).toList

def sieve(s: LazyList[Int]): LazyList[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

primes.take(10).toList

def sqrtLazy(x: Double): LazyList[Double] = {
  def improve(guess: Double): Double = (guess + x / guess) / 2
  lazy val guesses: LazyList[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtLazy(4811).filter(isGoodEnough(_, 4811)).take(1).toList.head