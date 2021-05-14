import scala.annotation.tailrec

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numerator: Int = x / g
  def denominator: Int = y / g

  def + (that: Rational): Rational = new Rational(
    numerator * that.denominator + that.numerator * denominator,
    denominator * that.denominator
  )

  def unary_- : Rational = new Rational(-numerator, denominator)

  def - (that: Rational): Rational = this + -that

  def < (that: Rational): Boolean = numerator * that.denominator < that.numerator * denominator

  def max(that: Rational): Rational = if (this < that) that else this

  override def toString = numerator + "/" + denominator
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x + y
x - y - z
y + y
x max y
new Rational(2)