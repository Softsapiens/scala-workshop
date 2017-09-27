package playground

// https://leanpub.com/fpmortals
object mortals_hkt extends App {

  import scala.concurrent.Future

  trait TerminalSync {
    def read(): String
    def write(t: String): Unit
  }

  trait TerminalAsync {
    def read(): Future[String]
    def write(t: String): Future[Unit]
  }


  // HKT power!

  trait Terminal[C[_]] {
    def read: C[String]
    def write(t: String): C[Unit]
  }

  // Id
  type Now[X] = X

  object TerminalSync extends Terminal[Now] {
    def read: String = "sync read"
    def write(t: String): Unit = println(s"sync write: ${t}")
  }

  object TerminalAsync extends Terminal[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global

    def read: Future[String] = Future {
        Thread.sleep(1500)
        "async read"
      }

    def write(t: String): Future[Unit] = Future {
      Thread.sleep(1500)
      println(s"async write: ${t}")
    }
  }

  trait Execution[C[_]] {
    def doAndThen[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  object ExecutionSync extends Execution[Now] {
    def doAndThen[A, B](c: Now[A])(f: A => Now[B]): Now[B] =
      f(c)

    def create[B](b: B): Now[B] = b
  }

  object ExecutionAsync extends Execution[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global

    def doAndThen[A, B](c: Future[A])(f: A => Future[B]): Future[B] =
      c flatMap f

    def create[B](b: B): Future[B] = Future(b)
  }


  def echo1[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
    e.doAndThen(t.read) { in: String =>
      e.doAndThen(t.write(in)) { _: Unit =>
        e.create(in)
      }
    }

  object Execution {
    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
              e.doAndThen(c)(f)
      def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
              e.doAndThen(c)(f andThen e.create)
    }
  }

  import Execution._

  def echo2[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    t.read.flatMap { in: String =>
        t.write(in).map { _: Unit =>
          in
        }
    }

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    for {
        in <- t.read
        _ <- t.write(in)
    } yield in


  echo(TerminalSync, ExecutionSync)

  echo(TerminalAsync, ExecutionAsync)

  println("Finished.")
}
