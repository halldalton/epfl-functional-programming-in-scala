import observers.{BankAccount, Consolidator}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a, b))

c.totalBalance

a deposit 20
b deposit 30

c.totalBalance