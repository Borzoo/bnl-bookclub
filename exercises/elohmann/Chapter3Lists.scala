import Chapter3Lists.Lijst.foldRight

import scala.annotation.tailrec

object Chapter3Lists {

  sealed trait Lijst[+E] {
    def show[E2]: String
  }

  case object Nil extends Lijst[Nothing] {
    def show[E2] = "Nil"
  }

  case class Cons[+E](h: E, t: Lijst[E]) extends Lijst[E] { // Why not extends Lijst[+E]

    // Question: how to override tostring generically

    // Question: How to write equals

    def show[E2]: String = {
      def doIt(l: Lijst[E2]): String = l match {
        case Nil => "Nil"
        case Cons(h, t) => h + ", " + doIt(t)
      }

      val s = doIt(this.asInstanceOf[Lijst[E2]])

      s"($s)"
    }

  }

  object Lijst {

    /** ********************* Exercise 3.1: *******************************/

    // 3rd case 1+2 = 3

    /** ********************* Exercise 3.2: *******************************/

    def tail[E](xs: Lijst[E]): Lijst[E] = xs match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def maybeTail[E](xs: Lijst[E]): Option[Lijst[E]] = xs match {
      case Nil => None
      case Cons(_, xs) => Some(xs)
    }

    /** ********************* Exercise 3.3: *******************************/

    def setHead[E](xs: Lijst[E], n: E): Lijst[E] = xs match {
      case Nil => Nil // There is no element to replace, otherwise Cons(n, Nil)
      case Cons(_, t) => Cons(n, t)
    }

    /** ********************* Exercise 3.4: *******************************/

    @tailrec
    def drop[E](xs: Lijst[E], n: Int): Lijst[E] = if (n < 1) xs
    else xs match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

    /** ********************* Exercise 3.5: *******************************/

    @tailrec
    def dropWhile[E](xs: Lijst[E], f: E => Boolean): Lijst[E] = xs match {
      case Nil => Nil
      case Cons(h, t) if !f(h) => Cons(h, t)
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }

    /** ********************* Exercise 3.6: *******************************/

    def init[E](l: Lijst[E]): Lijst[E] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil // Cut off last element
      case Cons(h, t) => Cons(h, init(t))
    }

    // We have no access to the (second to) last element, except walking the list

    /** ********************* Exercise 3.7: *******************************/

    def foldRight[A, B](l: Lijst[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

    def product(l: Lijst[Double]): Double = foldRight(l, 1.0)(_ * _)

    // This foldRight always recurs to the end of the list so no shortcutting

    def shortcutFoldRight[E, R](l: Lijst[E], z: R, s: E)(f: (E, R) => R): R = {
      print(".")
      l match {
        case Nil => z
        case Cons(x, _) if x == s => f(x, z) // Question: case Cons(s,_) => f(s,z) does not work
        case Cons(h, t) => f(h, shortcutFoldRight(t, z, s)(f))
      }
    }

    def shortcutProduct(l: Lijst[Double]): Double = shortcutFoldRight(l, 1.0, 0.0)(_ * _)

    /** ********************* Exercise 3.8: *******************************/

    // foldRight(Lijst(1,2,3), Nil:Lijst[Int])(Cons(_,_)) = Lijst(1,2,3)
    // They are right associative?

    /** ********************* Exercise 3.9: *******************************/

    def lengthR[E](xs: Lijst[E]): Int = foldRight(xs, 0)((_, l) => l + 1)

    /** ********************* Exercise 3.10: *******************************/

    @tailrec
    def foldLeft[E, R](l: Lijst[E], z: R)(f: (R, E) => R): R = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f) // Googled some
    }

    /** ********************* Exercise 3.11: *******************************/

    def sumL(l: Lijst[Int]): Int = Lijst.foldLeft(l, 0)(_ + _)

    def productL(l: Lijst[Double]): Double = Lijst.foldLeft(l, 1.0)(_ * _)

    def lengthL[E](l: Lijst[E]): Int = Lijst.foldLeft(l, 0)((l, _) => l + 1)

    /** ********************* Exercise 3.12: *******************************/

    def reverse[E](l: Lijst[E]): Lijst[E] = {
      foldLeft(l, Nil: Lijst[E])((l, e) => Cons(e, l))
    }

    /** ********************* Exercise 3.13: *******************************/

    def foldRightFromLeft[E, R](l: Lijst[E], z: R)(f: (E, R) => R): R = foldLeft(reverse(l), z)((a, b) => f(b, a))

    def foldLeftFromRight[E, R](l: Lijst[E], z: R)(f: (R, E) => R): R = foldRight(reverse(l), z)((a, b) => f(b, a))

    /** ********************* Exercise 3.14: *******************************/

    def append[E](xs: Lijst[E], ys: Lijst[E]): Lijst[E] = foldLeft(reverse(xs), ys)((l, e) => Cons(e, l))

    /** ********************* Exercise 3.15: *******************************/

    // fold append over lists
    def flatten[E](ls: Lijst[Lijst[E]]): Lijst[E] = foldLeft(ls, Nil: Lijst[E])((l1, l2) => append(l1, l2))

    /** ********************* Exercise 3.16: *******************************/

    def add1(xs: Lijst[Int]): Lijst[Int] = xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, add1(t))
    }

    def add2(xs: Lijst[Int]): Lijst[Int] = foldLeft(reverse(xs), Nil: Lijst[Int]) { (is, i) => Cons(i + 2, is) }

    def add3(xs: Lijst[Int]): Lijst[Int] = reverse(foldLeft(xs, Nil: Lijst[Int]) { (is, i) => Cons(i + 3, is) })

    def add4(xs: Lijst[Int]): Lijst[Int] = foldRight(xs, Nil: Lijst[Int]) { (i, is) => Cons(i + 4, is) }

    /** ********************* Exercise 3.17: *******************************/

    def doublesToStrings0(xs: Lijst[Double]): Lijst[String] = xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doublesToStrings(t))
    }

    def doublesToStrings(xs: Lijst[Double]): Lijst[String] = foldRight(xs, Nil: Lijst[String]) { (d, ss) => Cons(d.toString, ss) }

    /** ********************* Exercise 3.18: *******************************/

    def map0[A, B](xs: Lijst[A])(f: A => B): Lijst[B] = xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map0(t)(f))
    }

    // Use foldRightFromLeft is you want it tail recursive
    def map[A, B](xs: Lijst[A])(f: A => B): Lijst[B] = foldRight(xs, Nil: Lijst[B]) { (a, bs) => Cons(f(a), bs) }

    /** ********************* Exercise 3.19: *******************************/

    def filter0[A](l: Lijst[A])(f: A => Boolean): Lijst[A] = l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter0(t)(f))
        else filter0(t)(f)
    }

    def filter1[A](l: Lijst[A])(f: A => Boolean): Lijst[A] = foldLeft(reverse(l), Nil: Lijst[A]) { (as, a) =>
      if (f(a)) Cons(a, as) else as
    }

    def filter2[A](l: Lijst[A])(f: A => Boolean): Lijst[A] = foldRight(l, Nil: Lijst[A]) { (a, as) =>
      if (f(a)) Cons(a, as) else as
    }

    /** ********************* Exercise 3.20: *******************************/

    def flatMap[A, B](xs: Lijst[A])(f: A => Lijst[B]): Lijst[B] = foldRight(xs, Nil: Lijst[B]) {
      (a, bs) => append(f(a), bs)
    }

    /** ********************* Exercise 3.21: *******************************/

    def filter[A](l: Lijst[A])(f: A => Boolean): Lijst[A] = flatMap(l) { a =>
      if (f(a)) Lijst(a)
      else Nil
    }

    /** ********************* Exercise 3.22: *******************************/

    def addLists(as: Lijst[Int], bs: Lijst[Int]): Lijst[Int] = (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, addLists(ta, tb))
    }

    /** ********************* Exercise 3.23: *******************************/

    def zipWith[E](as: Lijst[E], bs: Lijst[E])(f: (E, E) => E): Lijst[E] = (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zipWith(ta, tb)(f))
    }

    /** ********************* Exercise 3.24: *******************************/

    def hasSubsequence[E](main: Lijst[E], sub: Lijst[E]): Boolean = (main, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a, _), Cons(b, _)) if a != b => false
      case (Cons(a, ta), Cons(b, tb)) if a == b => hasSubsequence(ta, tb)
    }

    // Utility

    def apply[E](xs: E*): Lijst[E] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))
  }

  def main(args: Array[String]): Unit = {

    val short = Lijst(1, 2, 3, 4, 5)
    val long = Lijst(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val ds0 = Lijst(1.0, 2.0, 0.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0)

    def lessThan(n: Int)(i: Int): Boolean = i < n

    Map(
      "tail(Nil)" -> Lijst.tail(Nil),
      "tail(short)" -> Lijst.tail(short),
      "setHead(Nil,0)" -> Lijst.setHead(Nil, 0),
      "setHead(short,0)" -> Lijst.setHead(short, 0),
      "drop(Nil,5)" -> Lijst.drop(Nil, 5),
      "drop(0,short)" -> Lijst.drop(short, 0),
      "drop(short,3)" -> Lijst.drop(short, 3),
      "drop(short,10)" -> Lijst.drop(short, 10),
      "dropWhile less then 4" -> Lijst.dropWhile(long, lessThan(4)),
      "init(long)" -> Lijst.init(long),
      "exercise 3.8" -> foldRight(Lijst(1, 2, 3), Nil: Lijst[Int])(Cons(_, _)),
      "reverse(Nil)" -> Lijst.reverse(Nil),
      "reverse(Lijst(3))" -> Lijst.reverse(Lijst(3)),
      "reverse(short)" -> Lijst.reverse(short),
      "append(short, short)" -> Lijst.append(short, short),
      "flatten(Nil)" -> Lijst.flatten(Nil),
      "flatten(Lijst(Nil))" -> Lijst.flatten(Lijst(Nil)),
      "flatten(Lijst(Nil, Nil))" -> Lijst.flatten(Lijst(Nil, Nil)),
      "flatten(Lijst(Nil, short, Nil))" -> Lijst.flatten(Lijst(Nil, short, Nil)),
      "flatten(Lijst(Nil, short,Nil, short)" -> Lijst.flatten(Lijst(Nil, short, Nil, short)),
      "add1(short)" -> Lijst.add1(short),
      "add2(short)" -> Lijst.add2(short),
      "add3(short)" -> Lijst.add3(short),
      "add4(short)" -> Lijst.add4(short),
      "doublesToStrings(ds0)" -> Lijst.doublesToStrings(ds0),
      """map(short)( "a" * _ )""" -> Lijst.map(short)("a" * _),
      "filter(short)(even)" -> Lijst.filter(long)(i => 0 == i % 2),
      "Lijst.flatMap(short){ i => Lijst(i,i )}" -> Lijst.flatMap(short) { i => Lijst(i, i) },
      "addLists(short, short)" -> Lijst.addLists(short, short),
      "addLists(long, short)" -> Lijst.addLists(long, short),
      "zipWith(short, long)( (a,b) => a * b )" -> Lijst.zipWith(short, long)((a, b) => a * b)
    ).foreach {
      case (m, l) => println(m + " = " + l.show)
    }

    Map(
      "maybeTail(Nil)" -> Lijst.maybeTail(Nil).map { l => s"Some(${l.show})" }.getOrElse("None"),
      "maybeTail(short)" -> Lijst.maybeTail(short).map { l => s"Some(${l.show})" }.getOrElse("None"),
      "product(ds0)" -> Lijst.product(ds0),
      "product(ds0)" -> Lijst.product(ds0),
      "shortcutProduct(ds0)" -> Lijst.shortcutProduct(ds0),
      "length(short)" -> Lijst.lengthR(short),
      "length(long)" -> Lijst.lengthR(long),
      "length(ds0)" -> Lijst.lengthR(ds0),
      "foldLeft(1,2,3,4,5)(+)" -> Lijst.foldLeft(short, 0)(_ + _),
      "foldLeft(1,2,3,4,5)(-)" -> Lijst.foldLeft(short, 0)(_ - _),
      "foldRight(1,2,3,4,5)(+)" -> Lijst.foldRight(short, 0)(_ + _),
      "foldRight(1,2,3,4,5)(-)" -> Lijst.foldRight(short, 0)(_ - _),
      "sumL(short)" -> Lijst.sumL(short),
      "lengthL(short)" -> Lijst.lengthL(short),
      "productL(ds0)" -> Lijst.productL(ds0),
      "foldRightFromLeft(short,0)(-)" -> Lijst.foldRightFromLeft(short, 0)(_ - _),
      "foldLeftFromRight(short,0)(-)" -> Lijst.foldLeftFromRight(short, 0)(_ - _),
      "hasSubsequence(long, short)" -> Lijst.hasSubsequence(long, short),
      "hasSubsequence(short, long)" -> Lijst.hasSubsequence(short, long),
      "hasSubsequence(short, short)" -> Lijst.hasSubsequence(short, short),
      "hasSubsequence(short, Nil)" -> Lijst.hasSubsequence(short, Nil),
      "hasSubsequence(Nil, Nil)" -> Lijst.hasSubsequence(Nil, Nil)
    ).foreach {
      case (m, s) => println(m + " = " + s)
    }

    println("Ok")
  }

}
