object Chapter3TreesBetter {

  sealed trait Tree[+E]

  case object Nil extends Tree[Nothing]

  case class Branch[E](value: E, left: Tree[E], right: Tree[E]) extends Tree[E]

  object Tree {

    /** ********************* Exercise 3.25: *******************************/

    def size[E](t: Tree[E]): Int = t match {
      case Nil => 0
      case Branch(_, l, r) => 1 + size(l) + size(r)
    }

    /** ********************* Exercise 3.26: *******************************/

    def maximum(t: Tree[Int]): Int = t match {
      case Nil => Int.MinValue // Oh oh. Option(Int)?
      case Branch(v, l, r) => v max (maximum(l) max maximum(r))
    }

    /** ********************* Exercise 3.27: *******************************/

    def depth[E](t: Tree[E]): Int = t match {
      case Nil => 0
      case Branch(_, l, r) => 1 + (depth(l) max depth(r))
    }

    /** ********************* Exercise 3.28: *******************************/

    def map[E, R](t: Tree[E])(f: E => R): Tree[R] = t match {
      case Nil => Nil
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }

    /** ********************* Exercise 3.29: *******************************/

    def fold[E, R](t: Tree[E], z: R)(f: (E, R, R) => R): R = t match {
      case Nil => z
      case Branch(v, l, r) => f(v, fold(l, z)(f), fold(r, z)(f))
    }

    def sizeF[E](t: Tree[E]): Int = fold(t, 0)((_, l, r) => 1 + r + l)

    def maximumF(t: Tree[Int]): Int = fold(t, Int.MinValue)((v, l, r) => v max (l max r))

    def depthF[E](t: Tree[E]): Int = fold(t, 0)((_, l, r) => 1 + (l max r))

    def mapF[E, R](t: Tree[E])(h: E => R): Tree[R] = fold(t, Nil: Tree[R]) { (v, l, r) => Branch(h(v), l, r) }

    // It folds both left and right

  }

  def test(): Boolean = {
    val subTree = Branch(2, Branch(-2, Nil, Nil), Nil)
    val tree = Branch(7, subTree, subTree)

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

  def main(args: Array[String]): Unit = {
    println(test())
  }

}
