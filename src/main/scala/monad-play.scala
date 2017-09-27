package playground

object monad_play extends App {
  import scala.language.higherKinds

  trait Monad[M[_]] {
    def point[A](a: => A): M[A]
    def bind[A, B](fa: M[A])(f: A => M[B]): M[B]
  }

  implicit class MonadPointer[A](a: A) {
    def point[M[_]: Monad] = implicitly[Monad[M]].point(a)
  }

  implicit class MonadWrapper[M[_]: Monad, A](t: M[A]) {
    private def m = implicitly[Monad[M]]
    def flatMap[B](f: A => M[B]): M[B] = m.bind(t)(f)
    def >>=[B](f: A => M[B]): M[B] = flatMap(f)
    def map[B](f: A => B): M[B] = m.bind(t)(a => m.point(f(a)))
    def flatten[B](implicit f: A => M[B]) = m.bind(t)(f)
  }

  sealed trait Z[T]
  case class MyZLeft[T](t: T) extends Z[T]
  case class MyZRight[T](t: T) extends Z[T]

  def swap[T](z: Z[T]) = z match {
    case MyZLeft(t) => MyZRight(t)
    case MyZRight(t) => MyZLeft(t)
  }

  implicit object ZIsMonad extends Monad[Z] {
    def point[A](a: => A): Z[A] = MyZRight(a)
    def bind[A, B](fa: Z[A])(f: A => Z[B]): Z[B] = fa match {
      case MyZLeft(t) => swap(f(t))
      case MyZRight(t) => swap(f(t))
    }
  }

  val z = 1.point[Z]
  val r = for {
    i <- z
    j <- (i + 1).point[Z]
    k = i + j
  } yield i * j * k

  println(r)

  type Ori[A] = (A, Unit)

  implicit object OriIsMonad extends Monad[Ori] {
    def point[A](a: => A): Ori[A] = (a, ())
    def bind[A, B](fa: Ori[A])(f: A => Ori[B]): Ori[B] = f(fa._1)
  }

  def f(x: Int): Ori[Int] = (x, ())
  def g(y: Int): Ori[Int] = (y * 2, ())

  val fc = for {
    a <- f(2)
    r <- g(a)
  } yield r

  println(fc)
}
