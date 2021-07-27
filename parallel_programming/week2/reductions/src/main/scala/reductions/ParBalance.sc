def balance(chars: Array[Char]): Boolean = {
  def checkChar(c: Char): Int = if (c == '(') 1 else if (c == ')') -1 else 0
  chars.scanLeft(0)((a, b) => a + checkChar(b)).tail forall (x => x >= 0)
}

val str1 = "()()".toArray

balance(str1)

val out = new Array[Int](str1.length)

def traverse(chars: Array[Char], idx: Int, until: Int, arg1: Int, arg2: Int, out: Array[Int]) = {
  def checkChar(c: Char): Int = if (c == '(') 1 else if (c == ')') -1 else 0
  var i = idx
  while (i < until) {
    out(i) = checkChar(chars(i))
    i = i + 1
  }
}

traverse(str1, 0, 3, 0, 0, out)
traverse(str1, 3, 4, 0, 0, out)
out