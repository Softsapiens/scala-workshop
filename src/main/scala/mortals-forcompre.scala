package playground

// https://leanpub.com/fpmortals
object mortals_forcompre extends App {

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scalaz._
import Scalaz._

def liftFutureOption[A](f: Future[Option[A]]) = OptionT(f)
def liftFuture[A](f: Future[A]) = f.liftM[OptionT]
def liftOption[A](o: Option[A]) = OptionT(o.pure[Future])
def lift[A](a: A)               = liftOption(Some(a))


def getA = Future(Option(5))
def getB = Future(Option(4))
def getC = Future(3)
def getD = Option(2)

val res = for {
  a <- liftFutureOption(getA)
  b <- liftFutureOption(getB)
  c <- liftFuture(getC)
  d <- liftOption(getD)
  e <- lift(10)
} yield e * (a * b) / (c * d)

//Thread.sleep(4000)
println(Await.result(res.run, 2.seconds))
//println(res.run)

}
