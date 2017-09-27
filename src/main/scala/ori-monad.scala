package playground

object ori_monad extends App {

  type I = Int
  type O = Int

  type Ori[A] = (A, Unit)

  def f(x: I): Ori[O] = (x, ())
  def g(y: O): Ori[O] = (y * 2, ())

  object case1 {
    implicit class OriMonad[A](val i: Ori[A]) {
      def flatMap[B](f: A => Ori[B]): Ori[B] = f(i._1)
      def map[B](f: A => B): Ori[B] = (f(i._1), ())

      def flatten[B](implicit f: A => Ori[B]) = flatMap(f)
    }
  }

  // Manual composition using basic functions
  val c = ((a: I) => f(a)._1) andThen g
  val c2 = f _ andThen { case (a, u) => g(a) }
  val c3 = f _ andThen ((a: Ori[O]) => g(a._1))

  // Composition using for-comprehensions
  {
    import case1._

    def fc_(i: I) =
      f(i).flatMap(g).map(_ * 2)

    def fc(i: I) = for {
      a <- f(i)
      r <- g(a)
    } yield r * 2

    def fc_explicit_(i: I) =
      new OriMonad(new OriMonad(f(i)).flatMap(g)).map(_ * 2)

    def fc_explicit(i: I) = for {
      a <- new OriMonad(f(i))
      r <- new OriMonad(g(a))
    } yield r * 2

    println(fc(2))
    println(fc_(2))
    println(fc_explicit_(2))
    println(fc_explicit(2))

    val a: Ori[Double] = (1.0, ())
    val b: Ori[Double] = (2.0, ())

    implicit class OriCanSum(val i1: Ori[Double]) {
      def +(i2: Ori[Double]) = (i1._1 + i2._1, ())
    }

    println(a + b)
  }
}
