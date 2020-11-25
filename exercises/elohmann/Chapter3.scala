

object Chapter3 {

  sealed trait Lijst[+E]

  case object Nil extends Lijst[Nothing]

  case class Cons[+E](h: E, t: Lijst[E]) extends Lijst[E] // Why not extends Lijst[+E]

  object Lijst {


    // Exercise 3.1:

    // 3rd case 1+2 = 3

    // Exercise 3.2

    def tail[E](xs: Lijst[E]): Lijst[E] = xs match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def maybeTail[E](xs: Lijst[E]): Option[Lijst[E]] = xs match {
      case Nil => None
      case Cons(_, xs) => Some(xs)
    }

    // Exercise 3.3

    def setHead[E](xs: Lijst[E], n: E): Lijst[E] = xs match {
      case Nil => Nil // There is no element to replace, otherwise Cons(n, Nil)
      case Cons(_, t) => Cons(n, t)
    }


    def apply[E](xs: E*): Lijst[E] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))

  }

  def main(args: Array[String]): Unit = {
    println("Ok")
  }

}
