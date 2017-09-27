package playground

import scalaz._
import scalaz.effect.{SafeApp, IO}

//http://degoes.net/articles/only-one-io
//https://gist.github.com/jdegoes/ce21e164b62bd0f4403e1fa76fd9051f
object AsyncPlay extends SafeApp {

  type Try[A] = Either[Throwable, A]
  type Callback[A] = (Try[A] => IO[Unit])

  final class Async[A](_register: Callback[A] => IO[Unit]) { self =>
    def register: Callback[A] => IO[Unit] = _register

    final def map[B](ab: A => B): Async[B] = new Async[B]( callback =>
      self.register {
        case Left(e) => callback(Left(e))
        case Right(a) => callback(Right(ab(a)))
      }
    )
    final def flatMap[B](afb: A => Async[B]): Async[B] = new Async[B](callback =>
      register {
        case Left(e) => callback(Left(e))
        case Right(a) => afb(a).register(callback)
      }
    )
  }
  object Async {
    final def apply[A](a: => A): Async[A] = new Async[A](callback => callback(Right(a)))

    final def fail[A](e: Throwable): Async[A] = new Async[A](callback => callback(Left(e)))
  }

  val asyncToIO = new NaturalTransformation[Async, IO] {
    def apply[A](fa: Async[A]): IO[A] = {
      for {
        ref     <- IO.newIORef[Try[A]](Left(new Error("No value")))
        counter <- IO(new java.util.concurrent.CountDownLatch(1))
        // Writing Try[A] into ref
        _       <- fa.register(v => ref.write(v).flatMap(_ => IO(counter.countDown())))
        _       <- IO(counter.await())
        // read a Try[A] from ref
        v       <- ref.read
        a       <- v match {
            case Left(e) => IO.throwIO(e)
            case Right(a) => IO(a)
          }
        } yield a
      }
    }

  override def runc: IO[Unit] = {
    val asy = Async[Int]{ val i = 1; println(s"Hi! returning ${i}"); i}.flatMap {i => Async[Unit] {println(s"getting ${i}")} }

    for {
      _ <- IO {println("async -> io")}
      a <- asyncToIO[Unit](asy)
      _ = println("aync runned")
    } yield a
  }
}
