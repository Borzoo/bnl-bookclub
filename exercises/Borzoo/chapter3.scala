import scala.annotation.tailrec

trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](xs: A*): List[A] = {
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
  }
}

// 1
// result is 3

// 2
// Other options include returning an Option or raising an error.
// This approach makes it easier to implement exercise 3
def tail[A](l: List[A]): List[A] =
  l match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

// 3
def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

// 4
@tailrec
def drop[A](l: List[A])(n: Int): List[A] =
  if (n <= 0) l
  else
    l match {
      case Nil        => Nil
      case Cons(_, t) => drop(t)(n - 1)
    }

// 5
@tailrec
def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = {
  l match {
    case Nil                => Nil
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case other              => other
  }
}

// 6
// It can't be implemented in constant time because we have to copy every element from second to last to first element
def init[A](l: List[A]): List[A] =
  l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

// 7
// We can short circuit foldRight. One way to do it is to accept a predicate and a default value and use it to stop traversing the list

// 8
// We'll get back a copy of the input list

// 9
def foldRight[A, B](as: List[A], init: B)(f: (A, B) => B): B = {
  as match {
    case Nil        => init
    case Cons(h, t) => f(h, foldRight(t, init)(f))
  }
}
def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

// 10
def foldLeft[A, B](l: List[A], init: B)(f: (A, B) => B): B = {
  @tailrec
  def loop(l: List[A], acc: B): B =
    l match {
      case Nil        => acc
      case Cons(h, t) => loop(t, f(h, acc))
    }

  loop(l, init)
}

// 11
def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
def product(l: List[Int]): Int = foldLeft(l, 0)(_ * _)
def length(l: List[Int]): Int = foldLeft(l, 0)((_, len) => len + 1)

// 12
def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)(Cons(_, _))

// 13
throw new Error

// 14
def append[A](l1: List[A], l2: List[A]): List[A] =
  foldRight(l1, l2)(Cons(_, _))

// 15
def flatten[A](ls: List[List[A]]): List[A] =
  foldRight[List[A], List[A]](ls, Nil)(append)

// 16
def add1(l: List[Int]): List[Int] =
  foldRight[Int, List[Int]](l, Nil)((a, b) => Cons(a + 1, b))

// 17
def doubleToString(l: List[Double]): List[String] =
  foldRight[Double, List[String]](l, Nil)((a, b) => Cons(a.toString, b))

// 18
def map[A, B](l: List[A])(f: A => B): List[B] =
  foldRight[A, List[B]](l, Nil)((a, b) => Cons(f(a), b))

// 19
def filter[A](l: List[A])(p: A => Boolean): List[A] =
  foldRight[A, List[A]](l, Nil)((a, b) => if (p(a)) Cons(a, b) else b)

def evens(l: List[Int]) = filter(l)(_ % 2 == 0)

// 20
def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

// 21
def filterByFlatMap[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(a => if (p(a)) List(a) else Nil)

// 22
def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  case _ => Nil
}

// 23
def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
  case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  case _ => Nil
}






