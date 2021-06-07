import signals._

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a = new BankAccount
val b = new BankAccount
val c = consolidated(List(a, b))

c()

a deposit 20
b deposit 30

c()

val xChange = Signal(246.00)
val inDollar = Signal(c() * xChange)
inDollar()

b withdraw 10

inDollar()