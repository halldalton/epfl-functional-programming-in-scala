def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s
  var sum: Int = 0
  while (i < t) {
    sum = sum + math.pow(a(i), p).toInt
    i = i + 1
  }
  sum
}

def pNorm(a: Array[Int], p: Double): Int =
  math.pow(sumSegment(a, p, 0, a.length), 1/p).toInt


val a = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

pNorm(a, 2)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {

}

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  math.pow(sum1 + sum2, 1/p).toInt
}

pNormTwoPart(a, 2)

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if (t -s < threshold)
    sumSegment(a, p, s, t)
  else {
    val m = s + (t - s)/2
    val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m , t))
    sum1 + sum2
  }
}

def pNormRec(a: Array[Int], p: Double): Int =
  math.pow(segmentRec(a, p, 0, a.length), 1/p).toInt