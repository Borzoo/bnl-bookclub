import scala.annotation.tailrec

object Chapter2 {

  //ex 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(idx: Int, acc: Int, next: Int): Int = {
      if (n == idx) return acc

      loop(idx + 1, acc + next, acc)
    }

    loop(0, 0, 1)
  }

  //ex 2.2
  @tailrec
  def isSorted[A](a: Array[A], pred: (A, A) => Boolean): Boolean = {
    if (a.length < 2) return true
    if (pred(a.head, a.tail.head)) isSorted(a.tail, pred) else false
  }

  @tailrec
  def iss[A](a: Array[A], pred: (A, A) => Boolean): Boolean = a match {
    case Array() | Array (_) => true
    case Array(x, y, _*) => if (pred(x, y)) iss(a.tail, pred) else false
  }

  //ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  //ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  //ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    val a = Array(1, 2)
    println(iss(a, (x: Int, y: Int) => { x <= y }))
  }
}

