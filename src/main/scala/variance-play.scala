package playground

object VariancePlay extends App {

  sealed trait Event

  object Event {
    case class Event1(ts: Long, msg: String) extends Event
    case class Event2(ts: Long, msg: String) extends Event
  }

  trait Producer[E] {
    def produce: E
  }

  import Event._

  case class SystemA() extends Producer[Event1] {
    def produce = Event1(System.currentTimeMillis, s"SystemA event")
  }

  case class SystemB() extends Producer[Event2] {
    def produce = Event2(System.currentTimeMillis, s"SystemB event")
  }


  def collect[E <: Event](ps: Producer[E]*) = {
    ps.map(_.produce)
  }


  collect(SystemA(), SystemA())
  collect(SystemB())

  // Comment out next line...
  //collect(SystemA(), SystemB(), SystemA())

  trait Consumer[E] {
    def consume(e: E): String
  }

  case class SystemC1() extends Consumer[Event1] {
    def consume(e: Event1) = s"SystemC1 consuming ${e}"
  }

  case class SystemC2() extends Consumer[Event2] {
    def consume(e: Event2) = s"SystemC2 consuming ${e}"
  }

  case class SystemC() extends Consumer[Event] {
    def consume(e: Event) = s"SystemC consuming ${e}"
  }

  def send[E](cs: Consumer[E]*)(e: E) =
    cs.map(_.consume(e))

  def send2One[E](c: Consumer[E])(e: E) =
    c.consume(e)

  val e1 = Event1(System.currentTimeMillis, "ev1")
  val e2 = Event2(System.currentTimeMillis, "ev2")

  send2One(SystemC())(e1)
  send2One(SystemC())(e2)
  send2One(SystemC1())(e1)
  send2One(SystemC2())(e2)
  //send2One(SystemC2())(e1)    // HKT type safe us

  println(s"""${ send(SystemC1())(e1) }""")
  println(s"""${ send(SystemC2())(e2) }""")
  println(s"""${ send(SystemC())(e1) }""")

  // Comment out next line
  //println(s"""${ send(SystemC(), SystemC1())(e1) }""")
  //println(s"""${ send(SystemC(), SystemC2())(e2) }""")
}
