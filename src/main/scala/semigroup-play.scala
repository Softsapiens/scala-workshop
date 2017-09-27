package playground

object SemigroupPlay extends App {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A: Semigroup] = implicitly[Semigroup[A]]
  }

  def :+:[A: Semigroup](a1: A, a2: A) =
    Semigroup[A].combine(a1, a2)

  implicit val intSemi = new Semigroup[Int] {
    def combine(x: Int, y: Int) = x + y
  }

  :+:(1, 2)

  case class Complex(x: Double, i: Double)

  object Complex {
    implicit val semiGroup = new Semigroup[Complex] {
      def combine(x: Complex, y: Complex) = Complex(x.x + y.x, x.i + y.i)
    }
  }

  import Complex._

  :+:(Complex(1, 1), Complex(1, -1))
}
