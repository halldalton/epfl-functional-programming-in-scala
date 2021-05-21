import week1._

object Trees {

  def leaves: Generator[Leaf] = for {
    x <- Generators.integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- Generators.booleans
    tree <- if (isLeaf) leaves else inners
  } yield tree

}

Generators.lists.generate
Trees.trees.generate