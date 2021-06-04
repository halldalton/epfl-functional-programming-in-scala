package week3

import scala.annotation.tailrec

trait Ordering[A] {
  def compare(a1: A, a2: A): Int
  def lt(a1: A, a2: A): Boolean = compare(a1, a2) <= 0
}

object Ordering {

  implicit val Int: Ordering[Int] = new Ordering[Int] {
    def compare(x: Int, y: Int): Int = if (x < y) -1 else if (x > y) 1 else 0
  }

  implicit val String: Ordering[String] = new Ordering[String] {
    def compare(s: String, t: String): Int = s.compareTo(t)
  }

  implicit def orderingList[A](implicit ord: Ordering[A]): Ordering[List[A]] = new Ordering[List[A]] {
    @tailrec
    def compare(xs: List[A], ys: List[A]): Int = (xs, ys) match {
      case (x :: xsTail, y :: ysTail) =>
        val c = ord.compare(x, y)
        if (c != 0) c else compare(xsTail, ysTail)
      case (Nil, Nil) => 0
      case (_, Nil)   => 1
      case (Nil, _)   => -1
    }
  }

  implicit def orderingPair[A, B](implicit orderingA: Ordering[A], orderingB: Ordering[B]): Ordering[(A, B)] = new Ordering[(A, B)] {
    def compare(pair1: (A, B), pair2: (A, B)): Int = {
      val firstCriteria = orderingA.compare(pair1._1, pair2._1)
      if (firstCriteria != 0) firstCriteria
      else orderingB.compare(pair1._2, pair2._2)
    }
  }

  def sort[A, B](elements: Seq[A])(criteria: A => B)(implicit ord: Ordering[B]): Seq[A] = {
    val n = elements.length / 2
    if (n == 0) elements
    else {
      def merge(xs: Seq[A], ys: Seq[A]): Seq[A] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(criteria(x), criteria(y)))
            x +: merge(xs1, ys)
          else
            y +: merge(xs, ys1)
      }
      val (fst, snd) = elements splitAt n
      merge(sort(fst)(criteria), sort(snd)(criteria))
    }
  }

}