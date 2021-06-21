package week1

class UniqueId {

  private val x = new AnyRef {}
  private var uidCount: Long = 0L

  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

}
