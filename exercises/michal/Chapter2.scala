import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def loop(idx: Int, acc: Int, next: Int): Int = {
      if (n == idx) return acc

      loop(idx + 1, acc + next, acc)
    }

    loop(0, 0, 1)
  }

  @tailrec
  def isSorted[A](a: Array[A], pred: (A, A) => Boolean): Boolean = {
    if (a.length < 2) return true
    if (pred(a.head, a.tail.head)) isSorted(a.tail, pred) else false
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}

