import week1.HelloThread

def helloThread(): Unit = {
  val t = new HelloThread
  val s = new HelloThread
  t.start()
  s.start()
  t.join()
  s.join()
}

helloThread()
helloThread()