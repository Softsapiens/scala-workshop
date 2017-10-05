package playground

object fs2KafkaPlay extends App {

  import fs2._
  import fs2.util.Async

  implicit val S: Strategy = Strategy.fromFixedDaemonPool(8, "4iq-poc-strategy")

  def log[A](f: A => String): Pipe[Task, A, A] = _.evalMap{ x =>
    Task {
      println(s"${f(x)}")
      x
    }
  }

  // Send event to Cyber Portal
  def portal[A]: Client => Pipe[Task, A, A] = r => _.evalMap { x =>
    Task {
      println(s"${Thread.currentThread.getName}> ${r} portal ${x}")
      x
    }
  }

  // Call back to 4iq
  def callback[A]: Client => Pipe[Task, A, A] = r => _.evalMap { x =>
    Task {
      println(s"${Thread.currentThread.getName}> ${r} callback ${x}")
      x
    }
  }

  type KMessage = String

  def source(r: Client)(to: String, p: Int, g: String): Stream[Task, KMessage] = {

    println(s"Connected to Kafka: topic ${to} partition ${p} - groupId ${g}")

    Stream.repeatEval(
      Task {
        // Here: the pull to the kafka java consumer
        Thread.sleep(500)
        List(s"${Thread.currentThread.getName}> msg1 from ${to}+${p} for ${g} at ${System.currentTimeMillis}"
               ,s"${Thread.currentThread.getName}> msg2 from ${to}+${p} for ${g} at ${System.currentTimeMillis}")
      })
  } flatMap Stream.emits through log(m => s"${m}")

  def sources = (r: Client) => source(r)("test.2", 0, "group-4iq") merge source(r)("test.2", 1, "group-4iq")

  type Client = String

  // Main flow
  def flow[F[_], R, A0, A1, A2](s: R => Stream[F, A0], p: R => Pipe[F, A0, A1], c: R => Pipe[F, A1, A2]): R => Stream[F, A2] =
    r => s(r) through p(r) through c(r)

  // Parallelize flow
  def parflow[F[_]: Async, R](t: Int)(f: R => Stream[F, _]): R => Stream[F, _] =
    (r: R) => concurrent.join(t)(Stream.emit(f(r)))

  // Acquire needed resources
  val acquire: Task[Client] = Task.delay {
    println("Acquiring resources")

    val client = "http-client"
    client
  }
  // Release all the acquired resources
  def release[A](r: A) = Task.delay { println("releasing " + r.toString) }

  def program[F[_]: Async, R](a: => F[R], s: R => Stream[F, _], r: R => F[Unit]): Stream[F, _] =
    Stream.bracket(a)(s(_), r(_))

  val t: Task[Unit] = program(acquire, parflow(1)(flow(sources, portal[KMessage], callback[KMessage])), release).run

  t.unsafeAttemptRun
}
