package week5

object listFun {

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: _ =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, count) => count + 1)

}
