package playground

// https://github.com/monix/monix
object MonixTaskPlay extends App {
  // From Future based DSLs to Monix Task Async ones

  case class Customer(id: Long, name: String)

  import scala.concurrent.{ ExecutionContext, Future }

  trait Database {
    def getClient(id: Long)(implicit ec: ExecutionContext): Future[Customer]
  }

  import monix.eval.Task

  trait DatabaseL {

    val db: Database

    def getClient(id: Long): Task[Customer] = Task.deferFutureAction {
      implicit scheduler =>
        db.getClient(id)
    }
  }

  // Some concurrency

  import java.time.LocalDateTime
  import scala.concurrent.Await
  import scala.concurrent.duration._

  def task(name: String) = Task.eval {
    val now = LocalDateTime.now
    println(s"${name} at ${now}")
    s"${name}-${now.toString.takeRight(6)}"
  }

  val batch1 = for {
    t1 <- task("t1").delayExecution(2.seconds)
    t2 <- task("t2").delayExecution(1.seconds)
    t3 <- task("t3")
  } yield s"batch1: ${t1} ${t2} ${t3}"

  val batch2 = Task.zipMap3(
    task("t1").delayExecution(2.seconds),
    task("t2").delayExecution(1.seconds),
    task("t3")) { (t1, t2, t3) => s"batch2: ${t1} ${t2} ${t3}" }

  import monix.execution.Scheduler.Implicits.global

  println(s"bacth1 execution begining at ${LocalDateTime.now}")
  val f1 = batch1.foreach(println)
  Await.result(f1, 5.seconds)

  println(s"bacth2 execution begining at ${LocalDateTime.now}")
  val f2 = batch2.foreach(println)
  Await.result(f2, 5.seconds)

}
