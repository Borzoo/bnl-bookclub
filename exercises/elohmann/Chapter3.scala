import Chapter3.Lijst.foldRight

import scala.annotation.tailrec

object Chapter3 {

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

    def map[A, B](xs: Lijst[A])(f: A => B): Lijst[B] = foldRight(xs, Nil: Lijst[B]) { (a, bs) => Cons(f(a), bs) }

    // Utility

    def apply[E](xs: E*): Lijst[E] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))
  }

  def main(args: Array[String]): Unit = {

    val short = Lijst(1, 2, 3, 4, 5)
    val long = Lijst(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    def display[E](m: String, l: Lijst[E]): Unit = println(s"$m: ${l.show[E]}")

    display("tail(Nil)", Lijst.tail(Nil))
    display("tail(short)", Lijst.tail(short))

    println("maybeTail(Nil): " + Lijst.maybeTail(Nil).map { l => s"Some(${l.show})" }.getOrElse("None"))
    println("maybeTail(short): " + Lijst.maybeTail(short).map { l => s"Some(${l.show})" }.getOrElse("None"))

    display("setHead(Nil,0)", Lijst.setHead(Nil, 0))
    display("setHead(short,0)", Lijst.setHead(short, 0))

    display("drop(Nil,5)", Lijst.drop(Nil, 5))
    display("drop 0", Lijst.drop(short, 0))
    display("drop short 3", Lijst.drop(short, 3))
    display("drop short 10", Lijst.drop(short, 10))

    def lessThan(n: Int)(i: Int): Boolean = i < n

    display("dropWhile less then 4", Lijst.dropWhile(long, lessThan(4)))

    display("init(long)", Lijst.init(long))

    val ds0 = Lijst(1.0, 2.0, 0.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0)

    println(s"product(ds0): ${Lijst.product(ds0)}")
    println(s"shortcutProduct(ds0): ${Lijst.shortcutProduct(ds0)}")

    println("exercise 3.8: ", foldRight(Lijst(1, 2, 3), Nil: Lijst[Int])(Cons(_, _)))

    println(s"length(short) = ${Lijst.lengthR(short)}")
    println(s"length(long) = ${Lijst.lengthR(long)}")
    println(s"length(ds0) = ${Lijst.lengthR(ds0)}")

    println(s"foldLeft(1,2,3,4,5)(+) = ${Lijst.foldLeft(short, 0)(_ + _)}")
    println(s"foldLeft(1,2,3,4,5)(-) = ${Lijst.foldLeft(short, 0)(_ - _)}")

    println(s"foldRight(1,2,3,4,5)(+) = ${Lijst.foldRight(short, 0)(_ + _)}")
    println(s"foldRight(1,2,3,4,5)(-) = ${Lijst.foldRight(short, 0)(_ - _)}")

    println(s"sumL(short) = ${Lijst.sumL(short)}")
    println(s"lengthL(short) = ${Lijst.lengthL(short)}")
    println(s"productL(ds0) = ${Lijst.productL(ds0)}")

    println(s"reverse(Nil) = ${Lijst.reverse(Nil).show}")
    println(s"reverse(Lijst(3)) = ${Lijst.reverse(Lijst(3)).show}")
    println(s"reverse(short) = ${Lijst.reverse(short).show}")

    println(Lijst.append(short, short).show)

    println(s"flatten(Nil) = ${Lijst.flatten(Nil)}")
    println(s"flatten(Lijst(Nil)) = ${Lijst.flatten(Lijst(Nil))}")
    println(s"flatten(Lijst(Nil, Nil)) = ${Lijst.flatten(Lijst(Nil, Nil))}")
    println(s"flatten(Lijst(Nil, short, Nil)) = ${Lijst.flatten(Lijst(Nil, short, Nil)).show}")
    println(s"flatten(Lijst(Nil, short,Nil, short) = ${Lijst.flatten(Lijst(Nil, short, Nil, short)).show}")

    println(s"foldRightFromLeft(short,0)(-) = ${Lijst.foldRightFromLeft(short, 0)(_ - _)}")

    println(s"foldLeftFromRight(short,0)(-) = ${Lijst.foldLeftFromRight(short, 0)(_ - _)}")

    println(s"add1(short) = ${Lijst.add1(short).show}")
    println(s"add2(short) = ${Lijst.add2(short).show}")
    println(s"add3(short) = ${Lijst.add3(short).show}")
    println(s"add4(short) = ${Lijst.add4(short).show}")

    println(s"doublesToStrings(ds0) = ${Lijst.doublesToStrings(ds0).show}")

    println(s"""map(short)( "a" * _ ) = ${Lijst.map(short)("a" * _).show}""")

    println("Ok")
  }

}
