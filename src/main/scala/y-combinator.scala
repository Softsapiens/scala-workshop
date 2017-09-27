package playground

object YCombinatorPlay extends App {
  def Y[A](f: (A => A) => (A => A)): (A => A) = f(Y(f))(_: A)

  def factorial = Y {
    f: (Int => Int) =>
      n: Int =>
        //println(s"$n")
        if (n == 1) 1
        else n * f(n - 1)
  }

  factorial(3)

  val factorial_y = Y[(Int, Int)] {
    f =>
      p: (Int, Int) =>
        val n = p._1
        val acc = p._2
        //println(s"$n $acc")
        if (n == 1) (n, acc)
        else {
          val p = (n - 1, n * acc)
          f(p)
        }
  }

  factorial_y((3, 1))._2

}
