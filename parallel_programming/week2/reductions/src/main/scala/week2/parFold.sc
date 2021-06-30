import week2._

def parallel[A](task1: A, task2: A) = ???

def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => f(reduce[A](l, f), reduce[A](r, f))
}

def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
  case Leaf(v) => v
  case Node(l, r) => {
    val (lV, rV) = parallel(reduce[A](l, f), reduce[A](r, f))
    f(lV, rV)
  }
}

def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def minus(x: Int, y: Int) = x - y

reduce[Int](tree, minus)