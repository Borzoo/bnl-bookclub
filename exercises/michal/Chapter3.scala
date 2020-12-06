import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]

case class Branch[+A](l: Tree[A], r: Tree[A]) extends Tree[A]

case class Leaf[+A](value: A) extends Tree[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  //ex 3.2
  def tailEx[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("empty list has no tail")
    case Cons(_, xs) => xs
  }

  //ex 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  //ex 3.3
  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil => throw new IllegalArgumentException("cannot replace first element of empty list")
    case Cons(_, xs) => Cons(newHead, xs)
  }

  //ex 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => if (n == 0) l else drop(xs, n - 1)
  }

  //ex 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  //ex 3.6
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(x, xs) => loop(xs, append(acc, x))
    }

    def append(l: List[A], e: A): List[A] = l match {
      case Nil => Cons(e, Nil)
      case Cons(x, xs) => Cons(x, append(xs, e))
    }

    loop(l, Nil)
  }

  def initTR[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(x, xs) => loop(xs, Cons(x, acc))
    }

    reverse(loop(l, Nil))
  }

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], r: List[A]): List[A] = l match {
      case Nil => r
      case Cons(x, xs) => loop(xs, Cons(x, r))
    }

    loop(l, Nil)
  }

  //ex 3.7
  // I don't think so, because foldRight traverses all the elements.
  // The only way I can think of is throwing an exception, but
  // that would be a questionable implementation.

  //ex 3.8
  // That foldRight can use constructors as any other functions?
  def ex38(): Unit = {
    def r = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

    println(r)
  }

  //ex 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, z) => z + 1)

  //ex 3.10
  def getBigList(size: Int = 10000): List[Int] = {
    var res = Nil: List[Int]
    (1 until size).foreach(i => res = Cons(i, res))
    res
  }

  def convinceYourself(): Unit = {
    val x = getBigList()

    println("fold left: ")
    println(foldLeft(x, 0)(_ + _))
    println("fold right:")
    println(foldRight(x, 0)(_ + _))
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  //ex 3.11
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)

  //ex 3.11 generalized
  def _sum[A](as: List[A])(implicit num: Numeric[A]): A = foldLeft(as, num.zero)(num.plus)

  def _product[A](as: List[A])(implicit num: Numeric[A]): A = foldLeft(as, num.one)(num.times)

  def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  //ex 3.12
  def reverseFold[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((res, el) => Cons(el, res))

  //ex 3.13
  def foldRight313[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    throw new RuntimeException("LOL")
  }

  //ex 3.14
  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, Cons(a, Nil))((el, res) => Cons(el, res))
  }

  //ex 3.15
  def flatten[A](x: List[List[A]]): List[A] = {
    foldRight(x, Nil: List[A])((l, acc) => foldRight(l, acc)((el, acc1) => Cons(el, acc1)))
  }

  //ex 3.16
  def plusOne(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil: List[Int])((el, acc) => Cons(el + 1, acc))
  }

  //ex 3.17
  def dtos(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((el, acc) => Cons(el.toString, acc))
  }

  //ex 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((el, acc) => Cons(f(el), acc))
  }

  //ex 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((el, acc) => if (f(el)) Cons(el, acc) else acc)
  }

  def ex319(): Unit = {
    val ints = List(1, 2, 3, 4)
    println(filter(ints)(_ % 2 == 0))
  }

  //ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  //ex 3.21
  def filterFM[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(el => if (f(el)) Cons(el, Nil) else Nil)
  }

  //ex 3.22 - not sure what should happen if the lists are of a different size
  def zipInt(as1: List[Int], as2: List[Int]): List[Int] = {
    @tailrec
    def loop(arg1: List[Int], arg2: List[Int], res: List[Int]): List[Int] = {
      arg1 match {
        case Cons(x, xs) => arg2 match {
          case Cons(y, ys) => loop(xs, ys, Cons(x + y, res))
          case _ => res
        }
        case _ => res
      }
    }

    reverse(loop(as1, as2, Nil: List[Int]))
  }

  //ex 3.23
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] =
    zipMap(as1, as2)(f)

  /*
  Initial solution
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def loop(arg1: List[A], arg2: List[A], res: List[A]): List[A] = {
      arg1 match {
        case Cons(x, xs) => arg2 match {
          case Cons(y, ys) => loop(xs, ys, Cons(f(x, y), res))
          case _ => res
        }
        case _ => res
      }
    }

    reverse(loop(as1, as2, Nil: List[A]))
  }*/
  def zipMap[A, B](as1: List[A], as2: List[A])(f: (A, A) => B): List[B] = {
    @tailrec
    def loop(arg1: List[A], arg2: List[A], res: List[B]): List[B] = {
      arg1 match {
        case Cons(x, xs) => arg2 match {
          case Cons(y, ys) => loop(xs, ys, Cons(f(x, y), res))
          case _ => res
        }
        case _ => res
      }
    }

    reverse(loop(as1, as2, Nil: List[B]))
  }

  //ex 3.24
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(arg1: List[A]): Boolean = length(sub) == length(filter(zipMap(arg1, sub)(_ == _))(_ == true))

    sub match {
      case Nil => true
      case _ => sup match {
        case Nil => false
        case Cons(_, xs) => if (startsWith(sup)) true else hasSubsequence(xs, sub)
      }
    }
  }

  //ex 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //ex 3.26
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => max(l) max max(r)
  }

  //ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  //ex 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //ex 3.29
  def fold[A, B](tree: Tree[A], acc: B)(f: (B, A) => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(acc, a)
    case Branch(l, r) => g(fold(l, acc)(f)(g), fold(r, acc)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int = fold(tree, 0)((b, _) => b + 1)(1 + _ + _)
  def maxFold(tree: Tree[Int]): Int = fold(tree, 0)(_ max _)(_ max _)
  def depthFold[A](tree: Tree[A]): Int = fold(tree, 0)((b, _) => b)((l, r) => 1 + (l max r))
  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree, null : Tree[B])((_, a) => Leaf(f(a)))(Branch(_, _))

  def sumFold(tree: Tree[Int]): Int = fold(tree, 0)(_ + _)(_ + _)

  def main(args: Array[String]): Unit = {
  }
}

