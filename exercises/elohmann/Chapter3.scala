

object Chapter3 {

  sealed trait Lijst[+E] {
    def show[E](): String
  }

  case object Nil extends Lijst[Nothing] {
    def show[E]() = "Empty"
  }

  case class Cons[+E](h: E, t: Lijst[E]) extends Lijst[E] { // Why not extends Lijst[+E]

    // Question: how to override tostring genericly

    // Question: How to write equals

     def show[E](): String = {
       def doIt(l: Lijst[E]): String = l match {
        case Nil => "Empty"
        case Cons(h, t) => h + ", " +  doIt(t)
      }

      val s = doIt(this.asInstanceOf[Lijst[E]])

      s"($s)"
    }

  }

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

    // Exercise 3.4

    def drop[E](xs: Lijst[E], n: Int): Lijst[E] = if (n < 1) xs
    else xs match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

    // Exercise 3.5

    def dropWhile[E](xs: Lijst[E], f: E => Boolean): Lijst[E] = xs match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case Cons(h, t) if !f(h) => Cons(h, t)
    }

    // Exercise 3.6

    def init[E](l: Lijst[E]): Lijst[E] = l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil // Cut off last element
      case Cons(h, t) => Cons(h, init(t))
    }

    // We have no access to the (second to) last element, except walking the list


    // Utility

    def apply[E](xs: E*): Lijst[E] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))

  }

  def main(args: Array[String]): Unit = {

    val short = Lijst(1, 2, 3, 4, 5)
    val long = Lijst(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    def display[E](m:String, l: Lijst[E]) : Unit = println( s"$m: ${l.show[E]()}" )

    display("tail(Nil)", Lijst.tail(Nil) )
    display("tail(short)", Lijst.tail(short) )

    println("maybeTail(Nil): " + Lijst.maybeTail(Nil).map { l => s"Some(${l.show})" }.getOrElse( "None") )
    println("maybeTail(short): " + Lijst.maybeTail(short).map { l => s"Some(${l.show})" }.getOrElse( "None") )

    display("setHead(Nil,0)", Lijst.setHead(Nil, 0))
    display("setHead(short,0)", Lijst.setHead(short, 0))

    display("drop(Nil,5)", Lijst.drop(Nil, 5))
    display("drop 0", Lijst.drop(short, 0))
    display("drop short 3", Lijst.drop(short, 3))
    display("drop short 10", Lijst.drop(short, 10))

    def lessThan(n: Int)(i: Int): Boolean = i < n

    display("dropWhile less then 4", Lijst.dropWhile(long, lessThan(4)))

    display("init(long)", Lijst.init(long))


    println("Ok")
  }

}
