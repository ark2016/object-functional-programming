object main {
  private def myMap[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case head :: tail => f(head) :: myMap(tail)(f)
  }

  private val comb: Int => (List[Int] => List[List[Int]]) = k =>
    def combinations(xs: List[Int]): List[List[Int]] =
      if (k == 0)
        List(Nil)
      else xs match {
        case Nil => Nil
        case head :: tail => myMap(comb(k - 1)(tail))(subList => head :: subList) ::: comb(k)(tail)
      }
    combinations

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4)
    val k = 2
    val result: List[List[Int]] = comb(k)(list)

    println(s"List: $list")
    println(s"k: $k")
    println(s"result: $result")
  }
}
