object Chapter2 extends App {

  // 2.1 --------------------------------------------------
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) n
    else fib(n - 1) + fib(n - 2)
  }

  def fibT(n: Int): Int = {
    @tailrec
    def calc(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else calc(n - 1, b, a + b)
    }

    calc(n, 0, 1)
  }

  /*
  fibT(3)
  = fibT(2, 1, 1)
  = fibT(1, 1, 2)
  = fibT(0, 2, 3)
  = 2
   */

  //  println(fibT(3))

  // 2.2 --------------------------------------------------
  val ordered: (Int, Int) => Boolean = (a, b) => a < b

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else loop(i + 1)
    }

    loop(0)
  }

  val a = Array(-1, 1, 3, 5)
  println(isSorted(a, ordered))


  // 2.3 --------------------------------------------------
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // 2.4 --------------------------------------------------
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5 --------------------------------------------------
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def inc: Int => Int = num => num + 1
  val plus2: Int => Int = compose(inc, inc)
  val plus3: Int => Int = compose(plus2, inc)
  println(plus2(1))
  println(plus3(1))

}
