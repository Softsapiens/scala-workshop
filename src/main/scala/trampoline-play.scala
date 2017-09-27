package playground

// http://blog.higher-order.com/assets/trampolines.pdf
object TrampolinePlay extends App {

  sealed trait Trampoline[A]

  object Trampoline {
    // The computation has finished with a result
    final case class Return[A](a: A) extends Trampoline[A]
    // The computation is suspended but we have a continuation 'k' to resume it.
    final case class Suspend[A](k: () => Trampoline[A]) extends Trampoline[A]
    // To get a Monad:
    // The result of the 'ta' computation is injected into the continuation 'k'
    final case class FlatMap[A, B](ta: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

    def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] = ta match {
      case Return(v) => Right(v)
      case Suspend(k) => Left(k)
      case FlatMap(ta, f) => ta match {
        case Return(v) => resume(f(v)) // When 'ta' computation is done, then the continuation is applied.
        case Suspend(k) => Left(() => FlatMap(k(), f)) // If 'ta' was suspended, we continu it. We run one step.
        case FlatMap(b, g) => resume(FlatMap(b, (x: Any) => FlatMap(g(x), f))) //: Trampoline[A])
      }
    }

    def runT[A](t: Trampoline[A]): A = resume(t) match {
      case Right(a) => a
      case Left(k) => runT(k())
    }
  }

  import Trampoline._

  val t1 = Suspend(() => Return(1))

  val r1 = runT(t1)
  println(s"""${r1}""")

  def rec(n: Long, i: Int, f: Int => Int): Trampoline[Int] = n match {
    case 0 => Return(i)
    case n => Suspend(() => rec(n - 1, f(i), f))
  }

  val trec = rec(1000000000, 0, (_: Int) + 1)

  val rRec = runT(FlatMap(trec, (v: Int) => Return(v * 2)))

  println(s"""${rRec}""")
}
