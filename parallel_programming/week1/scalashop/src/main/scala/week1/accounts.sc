import week1.{Account, UniqueId}

val u = new UniqueId

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500000, u)
val a2 = new Account(700000, u)

a1.getAmount()
a2.getAmount()

val t = startThread(a1, a2, 150000)
val s = startThread(a2, a1, 100000)

t.join()
s.join()

a1.getAmount()
a2.getAmount()
