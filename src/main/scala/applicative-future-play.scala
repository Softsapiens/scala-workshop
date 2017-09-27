package playground

import scala.concurrent.{Await, Future, ExecutionContext}
import scalaz.Applicative
import scalaz.syntax.applicative._
import scala.concurrent.duration._

object ApplicativeFuturePlay extends App {

  implicit def applicativeFuture(implicit ec: ExecutionContext) = new Applicative[Future] {
    def point[A](a: => A): Future[A] = Future.successful(a)
    def ap[A, B](fa: => Future[A])(f: => Future[(A) => B]): Future[B] = f.flatMap(fa.map(_))
  }

  def task(sleep: Long, value: Int)(implicit ec: ExecutionContext): Future[Int] = {
    Future {
      Thread.sleep(sleep)
      println(s""" task ${value} finishes!""")
      value
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  val expr = (task(100, 1) |@| task(200, 2) |@| task(700, 3)) { case r: (Int, Int, Int) => r.productIterator.toList }

  println(s""" Results =>  ${Await.result(expr, 1 second)}""")
}
