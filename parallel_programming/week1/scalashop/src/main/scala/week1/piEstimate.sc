import scala.util.Random

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

def mcCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble()
    val y = randomY.nextDouble()
    if (x*x + y*y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

def monteCarloPiPar(iter: Int): Double= {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mcCount(iter/4), mcCount(iter/4)),
    parallel(mcCount(iter/4), mcCount(iter - 3*(iter/4)))
  )
  4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}

monteCarloPiSeq(1000)