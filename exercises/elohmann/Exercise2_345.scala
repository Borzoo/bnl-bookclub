object Exercise2_345 {

  private def repeat(n: Int, s: String) : String = s * n

  // Exercise 2.3

  private def curry[A,B,C]  (f: (A, B) => C)  : A => (B => C) =
    a => b => f(a,b)

  // Exercise 2.3

  private def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)


  private def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  // Tests

  private def testCurry() : Boolean = {
    val repeatCurried: Int => String => String = curry(repeat)
    val repeatCopy: (Int, String) => String = uncurry(repeatCurried)

    Seq(
      (-1, "AB"),
      (0, "AB"),
      (2, "AB"),
      (9, "AB"),
    ).forall { case (n, s) => repeatCopy(n, s) == repeat(n, s) }
  }

  private def testCompose() : Boolean = {
    def inc(n: Int ): Int = n+1
    def twice(n: Int): Int = 2*n

    val incThenDouble = compose(twice, inc)
    val doubleThenInc = compose(inc, twice)

    incThenDouble(3) == 8 &&
    doubleThenInc(3) == 7 &&
    incThenDouble(0) == 2 &&
    doubleThenInc(0) == 1 &&
    true
  }

  private def testAll() : Boolean = {
    testCurry() && testCompose()
  }


  def main(args: Array[String]): Unit = if (testAll()) {
    println("Ok")
  } else {
    println("Fail")
  }

}



