trait Ring[A] {
  def plus(x: A, y: A): A
  def mult(x: A, y: A): A
  def inverse(x: A): A
  def zero: A
  def one: A
}

object Ring {
  implicit val ringInt: Ring[Int] = new Ring[Int] {
    def plus(x: Int, y: Int): Int = x + y
    def mult(x: Int, y: Int): Int = x * y
    def inverse(x: Int): Int = -x
    def zero: Int = 0
    def one: Int = 1
  }
  implicit val ringDouble: Ring[Double] = new Ring[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def mult(x: Double, y: Double): Double = x * y
    def inverse(x: Double): Double = -x
    def zero: Double = 0.0
    def one: Double = 1.0
  }
}

def plusAssociativity[A](x: A, y: A, z: A)(implicit ring: Ring[A]): Boolean =
  ring.plus(ring.plus(x, y), z) == ring.plus(x, ring.plus(y, z))

plusAssociativity(5, -2, 9)
plusAssociativity(5.3, -2.1, 9.54)