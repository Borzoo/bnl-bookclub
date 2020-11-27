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

    def sizeF[E](t: Tree[E]): Int = {
      def g[E](x: E): Int = 1

      def f(l: Int, r: Int): Int = r + l

      fold(t, 0)(g, f)
    }

    def maximumF(t: Tree[Int]): Int = {
      def g(x: Int): Int = x

      def f(l: Int, r: Int): Int = r max l

      fold(t, Int.MinValue)(g, f)
    }

    def depthF[E](t: Tree[E]): Int = {
      def g(x: E): Int = 1

      def f(l: Int, r: Int): Int = 1 + (r max l)

      fold(t, 0)(g, f)
    }

    def mapF[E, R](t: Tree[E])(h: E => R): Tree[R] = {
      def g(x: E): Tree[R] = Leaf(h(x))

      def f(l: Tree[R], r: Tree[R]): Tree[R] = Branch(l, r)

      fold(t, Nil: Tree[R])(g, f)
    }

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
        true
    }

  }

  def main(args: Array[String]): Unit = {
    println(Tree.test())
  }

}
