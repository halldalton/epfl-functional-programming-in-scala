val n = 7

def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
    isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x,y) <- xs zip ys) yield x * y).sum

val xs: List[Double] = List(1.0, 2.0, 3.0)
val ys: List[Double] = List(2.0, 3.0, 4.0)

scalarProduct(xs, ys)