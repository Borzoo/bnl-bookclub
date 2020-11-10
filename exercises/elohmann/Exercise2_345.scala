object Exercise2_345 {

  private def repeat(n: Int, s: String) : String = s * n

  // Exercise 2.3

  private def curry[A,B,C]  (f: (A, B) => C)  : A => (B => C) =
    a => b => f(a,b)

  // Exercise 2.3

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  // Tests

  private def testAll() : Boolean = {
    val repeatCurried: Int => String => String = curry(repeat)
    val repeatCopy: (Int, String) => String = uncurry(repeatCurried)

    Seq(
      (-1, "AB"),
      (0, "AB"),
      (2, "AB"),
      (9, "AB"),
    ).forall { case (n, s) => repeatCopy(n, s) == repeat(n, s) }
  }

  def main(args: Array[String]): Unit = if (testAll()) {
    println("Ok")
  } else {
    println("Fail")
  }

}



