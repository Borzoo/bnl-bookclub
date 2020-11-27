object Chapter3Trees {

  sealed trait Tree[+E]

  case object Nil extends Tree[Nothing]

  case class Leaf[E](value: E) extends Tree[E]

  case class Branch[E](left: Tree[E], right: Tree[E]) extends Tree[E]

  object Tree {

    /** ********************* Exercise 3.25: *******************************/

    def size[E](t: Tree[E]): Int = t match {
      case Nil => 0
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }

    /** ********************* Exercise 3.26: *******************************/

    def maximum(t: Tree[Int]): Int = t match {
      case Nil => Int.MinValue // Oh oh. Option(Int)
      case Leaf(i) => i
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    /** ********************* Exercise 3.27: *******************************/

    def depth[E](t: Tree[E]): Int = t match {
      case Nil => 0
      case Leaf(i) => 1
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

    /** ********************* Exercise 3.28: *******************************/

    def map[E, R](t: Tree[E])(f: E => R): Tree[R] = t match {
      case Nil => Nil
      case Leaf(e) => Leaf(f(e))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    /** ********************* Exercise 3.29: *******************************/

    // Question: is g really needed? Monad/Functor

    def fold[E, R](t: Tree[E], z: R)(g: E => R, f: (R, R) => R): R = t match {
      case Nil => z
      case Leaf(e) => g(e)
      case Branch(l, r) => f(fold(l, z)(g, f), fold(r, z)(g, f))
    }

    def sizeF[E](t: Tree[E]): Int =  fold(t, 0)( _ => 1, (l,r) => r + l)

    def maximumF(t: Tree[Int]): Int = fold(t, Int.MinValue)(identity, (l,r) => l max r)

    def depthF[E](t: Tree[E]): Int = fold(t, 0)(_ => 1, (l,r) => 1 + (l max r))

    def mapF[E, R](t: Tree[E])(h: E => R): Tree[R] = fold(t, Nil: Tree[R])( x => Leaf(h(x)), (l,r) => Branch(l, r) )

    // It folds both left and right

    def test(): Boolean = {
      val subTree = Branch(Leaf(1), Branch(Leaf(-2), Nil))
      val tree = Branch(subTree, subTree)

      Tree.size(tree) == 4 &&
        Tree.sizeF(tree) == 4 &&
        Tree.maximum(tree) == 1 &&
        Tree.maximumF(tree) == 1 &&
        Tree.depth(tree) == 4 &&
        Tree.depthF(tree) == 4 &&
        Tree.map(tree)(_ * 2) == Tree.mapF(tree)(_ * 2) &&
        Tree.maximum(Tree.map(tree)(_ * -1)) == 2
        true
    }

  }

  def main(args: Array[String]): Unit = {
    println(Tree.test())
  }

}
