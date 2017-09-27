package playground

// http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html

import scalaz._
import Scalaz._

object AuxPattern {

  trait Foo[A] {
    type B
    def value: B
  }

  object Foo {
    type Aux[A0, B0] = Foo[A0] { type B = B0 }

    implicit def fi = new Foo[Int] {
      type B = String
      val value = "Foo"
    }
    implicit def fs = new Foo[String] {
      type B = Boolean
      val value = false
    }

  }

  def ciao[T, R](t: T)
                (implicit f: Foo.Aux[T, R],
                          m: Monoid[R]): R = f.value
  val res = ciao(2)
  println(s"res: ${res}")
}

object AuxPattern2 {

  trait Apart[F]{
    type T
    type W[X]

    def apply(f: F): W[T]
  }

  object Apart {
    def apply[F](implicit apart: Apart[F]) = apart

    type Aux[FA, F[_], A] = Apart[FA]{ type T = A; type W[X] = F[X]  }

    implicit def mk[F[_], R]: Aux[F[R], F, R] = new Apart[F[R]]{
      type T = R
      type W[X] = F[X]

      def apply(f: F[R]): W[T] = f
    }
  }

  def mapZero[Thing, F[_], A](thing: Thing)
                             (implicit apart: Apart.Aux[Thing, F, A],
                                           f: Functor[F], 
                                           m: Monoid[A]): F[A] =
    f.map(apart(thing))(_ => m.zero)

  mapZero(Option(3))
}

object TypeLevel1 {
  import shapeless._
  import shapeless.ops.hlist.Length

  def length[T, R <: HList](t: T)
            (implicit g: Generic.Aux[T, R],
             l: Length[R]): l.Out = l()

  case class Foo(i: Int, s: String, b: Boolean)
  val foo = Foo(1, "", false)

  val res = length(foo)
  println(s"res: ${res} and its int length: ${Nat.toInt(res)}")
}

// http://www.vlachjosef.com/aux-pattern-evolution/
object aux_unwrap extends App {

  trait Unwrap[T[_], R] {
    type Out
    def apply(tr: T[R]): Out
  }

  object Unwrap extends UnwrapInstances {

    type Aux[T[_], R, Out0] = Unwrap[T, R] { type Out = Out0 }
  }

  trait UnwrapInstances extends UnwrapInstances1

  trait UnwrapInstances1 extends UnwrapInstances2 {
    implicit object listStringSize extends Unwrap[List, String] {
      type Out = Int
      def apply(tr: List[String]): Int = tr.size
    }

    implicit object listIntMax extends Unwrap[List, Int] {
      type Out = Int
      def apply(tr: List[Int]): Int = tr.max
    }
  }

  trait UnwrapInstances2 {
    implicit def faMax[A] = new Unwrap[List, A] {
      type Out = A
      def apply(tr: List[A]): A = tr.head
    }
  }

  def extractor[T[_], R, Out](in: T[R])(
    implicit
      unwrap: Unwrap.Aux[T, R, Out]
  ): Out =
    unwrap(in)

  trait Printer[T] {
    def apply(t: T): (String, T)
  }

  object Printer {
    implicit object stringPrinter extends Printer[String] {
      def apply(s: String): (String, String) = ("String: " + s, s)
    }

    implicit object intPrinter extends Printer[Int] {
      def apply(i: Int): (String, Int) = ("Int: " + i, i)
    }

    implicit def aPrinter[A] = new Printer[A] {
      def apply(i: A): (String, A) = (s"${i.getClass.getTypeName}: " + i, i)
    }
  }

  def extractorWithPrint[T[_], R, Out](in: T[R])(
    implicit unwrap: Unwrap.Aux[T, R, Out], withPrinter: Printer[Out]
  ): (String, Out) =
    withPrinter(unwrap(in))


  println(s"""${extractorWithPrint(List(1))}""")
  println(s"""${extractorWithPrint(List(2, 1))}""")
  println(s"""${extractorWithPrint(List("a", "b"))}""")
  println(s"""${extractorWithPrint(List(2.0, 1.0))}""")
}
