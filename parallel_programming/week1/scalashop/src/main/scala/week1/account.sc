import week1.{UniqueId, Account}

val uid = new UniqueId

val a = new Account(5000, uid)
val b = new Account(7000, uid)

a.getAmount
b.getAmount

a.transfer(b, 1000)

a.getAmount
b.getAmount

b.transfer(a, 2000)
a.transfer(b, 500)

a.getAmount
b.getAmount