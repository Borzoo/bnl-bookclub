object Exercise2_345 {

  // Exercise 2.3

  private def curry[A,B,C]  (f: (A, B) => C)  : A => (B => C) =
    a => b => f(a,b)

  private def repeat(n: Int, s: String) : String = s * n

  private def testCurry() : Boolean = {
      // A: Int
      // B: String
      // C: Seq[String]
      val repeat2 = curry( repeat )(2)
      val repeat0 = curry( repeat )(0)
      val repeat1 = curry( repeat )(1)
      val repeat5 = curry( repeat )(5)
      val repeatN1 = curry( repeat )(-7)

      repeat2("A") == "AA" &&
      repeat2("b") == "bb" &&
      repeat0("A") == "" &&
      repeat1("A") == "A" &&
      repeat5("A") == "AAAAA" &&
      repeatN1("x") == "" &&
      true
  }

  // Exercise 2.3

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  private def testUnCurry() : Boolean = {
    // A: Int
    // B: String
    // C: Seq[String]
    true
  }

  private def testAll() : Boolean = {
    testCurry() &&
    testUnCurry() &&
    true
  }

  def main(args: Array[String]): Unit = if (testAll()) {
    println("Ok")
  } else {
    println("Fail")
  }

}



