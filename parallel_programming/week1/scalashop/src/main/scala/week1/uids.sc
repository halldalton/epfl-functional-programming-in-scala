import week1.UniqueId

val u = new UniqueId

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield u.getUniqueId()
      println(uids)
    }
  }
  t.start()
  t
}

startThread()
startThread().join()