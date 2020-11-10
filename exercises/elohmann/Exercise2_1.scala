object Exercise2_1 {

  private def fibonacci(n: Int): Int = n match { // Exercise 2.1
    case _ if n <= 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
  }

  private def testFibonacci(): Boolean =
    Map(
     -1 -> 0,
      0 -> 0,
      1 -> 1,
      2 -> 1,
      3 -> 2,
      4 -> 3,
      5 -> 5,
      6 -> 8
    ).forall { case (k, v) => v == fibonacci(k) }


  def main(args: Array[String]): Unit = if (testFibonacci()) {
    println("Fibonacci Ok")
  } else {
    println("Fobonacci Fail")
  }

}
