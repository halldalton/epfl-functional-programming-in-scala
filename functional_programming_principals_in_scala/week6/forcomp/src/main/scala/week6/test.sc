val xs = Array(1, 2, 3, 44)
val s = "Hello World"

xs map (x => x * 2)
s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List('.', c))

xs.sum
xs.max

def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

isPrime(7)
isPrime(4)
isPrime(11)