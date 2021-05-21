package week1

import java.util.Random

object Generators {

  val integers: Generator[Int] = new Generator[Int] {
    val rand = new Random
    def generate: Int = rand.nextInt()
  }

  val booleans: Generator[Boolean] = for (x <- integers) yield x > 0

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, hi=xs.length)) yield xs(idx)

  def emptyLists: Generator[Nil.type] = single(Nil)

  def nonEmptyLists: Generator[List[Int]] = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

}
