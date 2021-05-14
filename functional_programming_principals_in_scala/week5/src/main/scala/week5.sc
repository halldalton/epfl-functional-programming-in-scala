import week5.mergeSort
import week5.listFun

val fruit = List("bananas", "apples", "strawberries", "oranges", "kiwis", "pears")
val nums = List(2, -10, 3, 5, 1, -9, 0, 2, 4, 1)
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val chars = List("a", "a", "a", "b", "c", "c", "a")
val empty = List()

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => xs
  case y :: ys => y match {
    case z :: zs => z :: flatten(zs) ::: flatten(ys)
    case _ => y :: flatten(ys)
  }
}

def squareList(xs: List[Int]): List[Int] = xs match {
  case List() => xs
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

removeAt(1, fruit)
removeAt(1, nums)
removeAt(1, diag3)
removeAt(1, empty)

flatten(diag3)
flatten(List(List(1, 1), 2, List(3, List(5, 8))))

squareList(nums)
squareList2(nums)

mergeSort.msort(nums)
mergeSort.msort(fruit)

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

listFun.pack(chars)
listFun.encode(chars)

listFun.mapFun(nums, (x: Int) => x * -1)
listFun.lengthFun(nums)