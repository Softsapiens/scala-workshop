package playground

// https://leanpub.com/fpmortals
trait Ordering[T] {
  def compare(x: T, y: T): Int
  def lt(x: T, y: T): Boolean = compare(x, y) < 0
  def gt(x: T, y: T): Boolean = compare(x, y) > 0
}

trait Numeric[T] extends Ordering[T] {
  def plus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def zero: T
  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
}

object Numeric {
  def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

  object ops {
    implicit class NumericOps[T](t: T)(implicit N: Numeric[T]) {
      def +(o: T): T = N.plus(t, o)
      def *(o: T): T = N.times(t, o)
      def unary_- = N.negate(t)
      def abs: T = N.abs(t)
      // duplicated from Ordering.ops
      def <(o: T) = N.lt(t, o)
      def >(o: T) = N.gt(t, o)
    }
  }
}

object instances {

  import Numeric.ops._

  implicit val NumericDouble: Numeric[Double] = new Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def zero: Double = 0.0
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
    // optimised
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def abs(x: Double): Double = java.lang.Math.abs(x)
  }

  import java.math.{ BigDecimal => BD }
  implicit val NumericBD: Numeric[BD] = new Numeric[BD] {
    def plus(x: BD, y: BD): BD = x.add(y)
    def times(x: BD, y: BD): BD = x.multiply(y)
    def negate(x: BD): BD = x.negate
    def zero: BD = BD.ZERO
    def compare(x: BD, y: BD): Int = x.compareTo(y)
  }

  final case class Complex[T](r: T, i: T)

  implicit def numericComplex[T: Numeric]: Numeric[Complex[T]] =
    new Numeric[Complex[T]] {
      type CT = Complex[T]
      def plus(x: CT, y: CT): CT = Complex(x.r + y.r, x.i + y.i)
      def times(x: CT, y: CT): CT =
        Complex(x.r * y.r + (-x.i * y.i), x.r * y.i + x.i * y.r)
      def negate(x: CT): CT = Complex(-x.r, -x.i)
      def zero: CT = Complex(Numeric[T].zero, Numeric[T].zero)
      def compare(x: CT, y: CT): Int = {
        val real = (Numeric[T].compare(x.r, y.r))
        if (real != 0) real
        else Numeric[T].compare(x.i, y.i)
      }
    }
}

object functionarity {
  import Numeric.ops._

  def signOfTheTimes[T: Numeric](t: T): T = -(t.abs) * t
}

object NumericPlay extends App {

  import instances._
  import functionarity._

  val t1 = signOfTheTimes(new Complex(5.0, 1.0))

  println(s"t1= ${t1}")
}
