import week1.UniqueId

def startThread(uniqueId: UniqueId) = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield uniqueId.getUniqueId()
      print(uids)
    }
  }
  t.start()
  t
}

val uid = new UniqueId

val t = startThread(uid)
val s = startThread(uid)

t.join()
s.join()