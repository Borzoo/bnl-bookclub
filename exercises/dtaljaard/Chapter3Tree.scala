package dt.fpinscala.ch3

import dt.fpinscala.ch3.Tree._


sealed trait Tree[+A]

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(leafFun: A => B)(branchFun: (B, B) => B): B = t match {
    case Leaf(v) => leafFun(v)
    case Branch(l, r) => branchFun(fold(l)(leafFun)(branchFun), fold(r)(leafFun)(branchFun))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximumViaFold[A](t: Tree[Int]): Int = fold(t)(a => a)((l, r) => l.max(r))

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((l, r) => 1 + l.max(r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}

object Chapter3TreeExercises extends App {
  val t1 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  // 3.25 ------------------------------------------------
  // See size function
  assert(size(t1) == 7)

  // 3.26 ------------------------------------------------
  // See max function
  assert(maximum(t1) == 4)

  // 3.27 ------------------------------------------------
  // See depth function
  assert(depth(t1) == 2)

  // 3.28 ------------------------------------------------
  // See map function
  val t2 = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
  assert(map(t1)(_ * 2) == t2)

  // 3.29 ------------------------------------------------
  // See fold function
  assert(sizeViaFold(t1) == 7)
  assert(maximumViaFold(t1) == 4)
  assert(depthViaFold(t1) == 2)
  assert(mapViaFold(t1)(_ * 2) == t2)

}
