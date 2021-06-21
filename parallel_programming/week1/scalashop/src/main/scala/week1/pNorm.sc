def task[A](c: => A): Task[A] = ??? // not implemented in lecture

trait Task[A] {
  def join: A
}

implicit def getJoin[T](x: Task[T]): T = x.join

def parallel[A, B](cA: => A, cB: => B): (A, B) = {
  val tB: Task[B] = task {cB}
  val tA: A = cA
  (tA, tB.join)
}

def power(x: Int, p: Double): Int = math.pow(math.abs(x), p).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s; var sum: Int = 0
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

def pNorm(a: Array[Int]): Int = {
  val p: Int = a.length
  val pInverse: Double = 1.0 / p
  power(sumSegment(a, p, 0, p), pInverse)
}

def pNormTwoPart(a: Array[Int]): Int = {
  val p: Int = a.length
  val pInverse: Double = 1.0 / p
  val m = p / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m),
                              sumSegment(a, p, m, p))
  power(sum1 + sum2, pInverse)
}

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if (t - s < 2)
    sumSegment(a, p, s, t)
  else {
    val m = s + (t - s) / 2
    val (sum1, sum2) = parallel(segmentRec(a, p, s, m),
                                segmentRec(a, p, m, t))
    sum1 + sum2
  }
}

def pNormRec(a: Array[Int]): Int = {
  val p: Int = a.length
  val pInverse: Double = 1.0 / p
  power(segmentRec(a, p, 0, p), pInverse)
}

val a = Array(3, 4)

pNorm(a)
pNormTwoPart(a)