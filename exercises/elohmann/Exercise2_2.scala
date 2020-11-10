object Exercise2_2 {

  private def ascending(a: Int, b:Int) : Boolean = a <= b

  private def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(bs: Array[A]) : Boolean = bs.length < 2 || ordered(bs(0), bs(1)) && loop(bs.drop(1))
    loop(as)
  }

  @annotation.tailrec
  private def isSorted2[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as.length < 2 || ordered(as(0), as(1)) && isSorted2( as.drop(1), ordered)

  private def testIsSorted() : Boolean = {
    true == isSorted(Array(9), ascending) &&
    true == isSorted(Array(1,2), ascending) &&
    true == isSorted(Array(1,2,3), ascending) &&
    true == isSorted(Array(1,1,1,1,1), ascending) &&
    true == isSorted(Array(1,1,1,1,2), ascending) &&
    false == isSorted(Array(2,1), ascending) &&
    false == isSorted(Array(3,4,1,2), ascending) &&
    true == isSorted(Array(), ascending)
  }

  def main(args: Array[String]): Unit = if (testIsSorted()) {
    println("Ok")
  } else {
    println("Fail")
  }

}


