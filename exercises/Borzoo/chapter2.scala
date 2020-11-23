import scala.annotation.tailrec

// 1
def fib(n: Int): Int = {

  @tailrec
  def go(a: Int, b: Int, c: Int): Int = {
    if(c == 0)
      a
    else
      go(b, a + b, c - 1)
  }

  go(0, 1, n)
}

// 2
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

  @tailrec
  def go(i: Int): Boolean = {
    if(i > as.length - 2)
      true
    else if(ordered(as(i), as(i + 1)))
      go(i + 1)
    else
      false
  }

  go(0)
}

// 3
def curry[A,B,C](f: (A, B) => C): A => B => C = {
  a => b => f(a, b)
}

// 4
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

// 5
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}



