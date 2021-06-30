import week2._

def parallel(task1: Unit, task2: Unit) = ??? // not implemented in lecture

def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  var i = left
  while (i < right) {
    out(i) = f(inp(i))
    i = i + 1
  }
}

def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  val threshold = 32
  if (right - left < threshold)
    mapASegSeq(inp, left, right, f, out)
  else {
    val mid = left + (right - left) / 2
    parallel(mapASegPar(inp, left, mid, f, out), mapASegPar(inp, mid, right, f, out))
  }
}

val in = Array(2, 3, 4, 5, 6)
val out = Array(0, 0, 0, 0, 0)
def f(x: Int) = x * x
mapASegSeq(in, 1, 3, f, out)
out