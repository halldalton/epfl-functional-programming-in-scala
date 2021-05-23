package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h1: H): List[Int] = {
    if (isEmpty(h1)) Nil
    else findMin(h1) :: toList(deleteMin(h1))
  }

  // Adding a single element to an empty heap, and then removing this element, should yield the element in question
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // For any heap, adding the minimal element, and then finding it, should return the element in question
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val minimum = math.min(a, b)
    val h = insert(b, insert(a, empty))
    findMin(h) == minimum
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("gen2") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  property("gen3") = forAll { (h: H) =>
    toList(h) == toList(h).sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min3") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin(meld(h1, h2)) == findMin(h2)
    else if (isEmpty(h2)) findMin(meld(h1, h2)) == findMin(h1)
    else findMin(meld(h1, h2)) == findMin(h1) || findMin(meld(h1, h2)) == findMin(h2)
  }

  // the set of concatenated values from any two heaps should equal to the set of values of those two heaps melded together
  property("meld1") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    (toList(h1) ::: toList(h2)).toSet == toList(h3).toSet
  }

}