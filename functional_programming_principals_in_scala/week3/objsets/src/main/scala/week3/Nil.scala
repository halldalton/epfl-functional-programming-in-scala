package week3

import java.util.NoSuchElementException

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.Head")
  def tail: Nothing = throw new NoSuchElementException("Nil.Tail")
}
