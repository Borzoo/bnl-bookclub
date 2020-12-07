package dt.fpinscala.ch3

import dt.fpinscala.ch3.List._

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // previously init
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // throw an exception instead?
    case Cons(_, xs) => xs
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, Cons(x, xs))
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => l
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) if !f(x) => Cons(x, xs)
    case Cons(_, xs) => dropWhile(xs, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productViaFL(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lengthViaFL[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)

  //  def foldRightViaFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, Nil: List[A]) ()
  //  def foldLeftViaFR[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, Nil: List[A]) ()

  def addOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairs(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairs(xs, ys))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  @tailrec
  def same[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => same(xs, ys)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sup == sub
    case _ if same(sup, sub) => true
    case Cons(_, xs) => hasSubsequence(xs, sub)
  }
}

object Chapter3ListExercises extends App {
  // 3.1 ------------------------------------------------
  /**
   * val x = List(1,2,3,4,5) match {
   * case Cons(x, Cons(2, Cons(4, _))) => x
   * case Nil => 42
   * case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
   * case Cons(h, t) => h + sum(t)
   * case _ => 101
   * }
   */
  // Result of the match expression: 3

  // 3.2 ------------------------------------------------
  // See tail function
  val tailTest = List(1, 2, 3, 4, 5)
  assert(tail(tailTest) == List(2, 3, 4, 5))

  // 3.3 ------------------------------------------------
  // sse setHead function
  val setHeadTest = List(2, 3, 4, 5)
  assert(setHead(1, setHeadTest) == List(1, 2, 3, 4, 5))

  // 3.4 ------------------------------------------------
  // see drop function
  val dropTest = List(1, 2, 3, 4, 5)
  assert(drop(dropTest, 2) == List(3, 4, 5))
  assert(drop(dropTest, 0) == dropTest)

  // 3.5 ------------------------------------------------
  // see dropWhile function
  val dropWhileTest = List(1, 2, 3, 4, 5)
  assert(dropWhile(dropWhileTest, (x: Int) => x <= 2) == List(3, 4, 5))
  assert(dropWhile(dropWhileTest, (x: Int) => x > 10) == dropWhileTest)

  // 3.6 ------------------------------------------------
  // see init function
  val initTest = List(1, 2, 3, 4, 5)
  assert(init(initTest) == List(1, 2, 3, 4))

  // 3.7 ------------------------------------------------
  // Can product implemented using foldRight, immediately halt the recursion when it encounters 0.0?
  // No, note that foldRight must traverse all the way to the end of the list (pushing frames
  // onto the call stack as it goes) before it can begin collapsing it. We evaluate the function's argument.

  // 3.8 ------------------------------------------------
  // What happens when you pass Nil and Cons themselves to foldRight?
  val s = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  assert(s == List(1, 2, 3))
  // We get back the list themselves.

  // 3.9 ------------------------------------------------
  // see length function
  assert(length(List(1, 2, 3)) == 3)

  // 3.10 ------------------------------------------------
  // see foldLeft function

  // 3.11 ------------------------------------------------
  // see sum, product & length functions
  assert(sumViaFL(List(1, 2, 3)) == 6)
  assert(productViaFL(List(1, 2, 4)) == 8)
  assert(lengthViaFL(List(1, 2, 3)) == 3)

  // 3.12 ------------------------------------------------
  // see reverse function
  assert(reverse(List(1, 2, 3)) == List(3, 2, 1))

  // 3.13 ------------------------------------------------
  // write foldLeft in terms of foldRight

  // write foldRight in terms of foldLeft

  // 3.14 ------------------------------------------------
  // see append function
  assert(append(List(1, 2), List(1, 3)) == List(1, 2, 1, 3))

  // 3.15 ------------------------------------------------
  // see concat function
  assert(concat(List(List(1, 2), List(1, 3))) == List(1, 2, 1, 3))

  // 3.16 ------------------------------------------------
  // see addOne function
  assert(addOne(List(1, 2, 3)) == List(2, 3, 4))

  // 3.17 ------------------------------------------------
  // see doubleToString function
  assert(doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))

  // 3.18 ------------------------------------------------
  // see map function
  assert(map(List(1.0, 2.0, 3.0))(_.toString) == List("1.0", "2.0", "3.0"))

  // 3.19 ------------------------------------------------
  // see filter function
  assert(filter(List(1, 2, 3))(_ % 2 == 0) == List(2))

  // 3.20 ------------------------------------------------
  // see flatMap function
  assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

  // 3.21 ------------------------------------------------
  // see flatMapUsingFilter function
  assert(filterUsingFlatMap(List(1, 2, 3))(_ % 2 == 0) == List(2))

  // 3.22 ------------------------------------------------
  // see addPairs function
  assert(addPairs(List(1, 2, 3), List(1, 2, 3)) == List(2, 4, 6))

  // 3.23 ------------------------------------------------
  // see zipWith function
  assert(zipWith(List(1, 2, 3), List(1, 2, 3))(_ + _) == List(2, 4, 6))

  // 3.24 ------------------------------------------------
  // See hasSubsequence function
  assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
  assert(!hasSubsequence(List(1, 2, 3, 4), List(5)))
}
