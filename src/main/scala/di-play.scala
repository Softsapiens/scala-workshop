package playground

object ClassDI extends App {

  type Msg = String
  type Sink = Msg => Unit
  type Logger = String => Unit

  class SendService(sink: Sink, log: Logger) {
    def send(m: Msg): Unit = {
      log(s"""Sending ${m} using ${sink}""")
      sink(m)
    }
  }

  // Parametricity: abstraction over types
  class SendService_[S, L](sink: S, log: L)(implicit ev2: S => Sink, ev3: L => Logger) {
    def send(m: Msg): Unit = {
      log(s"""Sending ${m} using ${sink}""")
      sink(m)
    }
  }

  new SendService(println, println).send("Hola")

  val sinker: Sink = println
  val logger: Logger = println

  new SendService_(sinker, logger).send("Hola")
}


object FunctionDI extends App {

  type Msg = String
  type Sink = Msg => Unit
  type Logger = String => Unit

  def send(m: Msg)(sink: Sink, log: Logger): Unit = {
    log(s"""Sending ${m} using ${sink}""")
    sink(m)
  }

  // Parametricity <-abstract over function parameters
  // Use the valid abstraction with less constraints
  // DI by parameters
  def send_[M, S, L](m: M)(sink: S, log: L)
           (implicit ev1: M => Msg, ev2: S => Sink, ev3: L => Logger): Unit = {
    log(s"""Sending ${m} using ${sink}""")
    sink(m)
  }

  send("hola")(println, println)

  val sinker: Sink = println
  val logger: Logger = println

  send_("hola": Msg)(sinker, logger)
}

object TypeclassDI extends App {

  trait Sink[F[_], M] {
    def apply(m: M): F[Unit]
  }

  trait Logger[F[_]] {
    def apply(s: String): F[Unit]
  }

  type Msg = String

  def send[F[_]: Sink[?[_], M]: Logger, M](m: M): F[Unit] = {
    val sink = implicitly[Sink[F, M]]
    val log = implicitly[Logger[F]]

    log(s"""Sending ${m} using ${sink}""")
    sink(m)
  }

  type Id[A] = A

  implicit val logger = new Logger[Id] {
    def apply(s: String) = println(s)
  }

  implicit val sinker = new Sink[Id, Msg] {
    def apply(m: Msg) = println(m)
  }

  // implicitly
  send[Id, Msg]("hola")

  // explicitly
  send[Id, Msg]("hola")(sinker, logger)
}
