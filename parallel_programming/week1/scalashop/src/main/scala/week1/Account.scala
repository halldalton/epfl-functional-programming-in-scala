package week1

class Account(private var amount: Int = 0, uniqueId: UniqueId) {

  val uid = uniqueId.getUniqueId()

  def getAmount: Int = amount

  private def lockAndTransfer(target: Account, n: Int) = {
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  }

  def transfer(target: Account, n: Int): Unit = {
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
  }

}
