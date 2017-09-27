package playground

object optics {
  object monomorphic {
    // S is a Product data structure
    // A is the target part of a product S
    trait Lens[S, A] {
      def get(s: S): A
      def set(a: A)(s: S): S

      // Extra
      def modify(f: A => A)(s: S): S =
        set((f compose get)(s))(s)
    }

    // S is a Coproduct data structure or Sum type
    // A is the target case of the coproduct S
    trait Prism[S, A] {
      def getOption(s: S): Option[A]
      def reverseGet(a: A): S // Why can we generate S only from A ?
    }
  }

  object polymorphic {
    trait PLens[S, T, A, B] {
      def get(s: S): A
      def set(b: B)(s: S): T
    }

    type Lens[S, A] = PLens[S, S, A, A]

    object Lens {
      def apply[S, A](_get: S => A)(_set: A => S => S) =
        new Lens[S, A] {
          def get(s: S): A = _get(s)
          def set(a: A)(s: S): S = _set(a)(s)
        }

      def asOptional[S, A](l: Lens[S, A]): Optional[S, A] =
        Optional[S, A](s => Some(l.get(s)))(l.set)

      def composeLens[S, A, B](fst: Lens[S, A])(snd: Lens[A, B]): Lens[S, B] =
        Lens[S, B](fst.get _ andThen snd.get)(b => s => fst.set(snd.set(b)(fst.get(s)))(s))

      def composePrism[S, A, B](fst: Lens[S, A])(snd: Prism[A, B]): Optional[S, B] =
        Optional[S, B](fst.get _ andThen snd.getOption)(b => fst.set(snd.reverseGet(b))(_))

      def composePrism_[S, A, B](fst: Lens[S, A])(snd: Prism[A, B]): Prism[S, B] =
        ??? // Exercice

      def composeOptional[S, A, B](fst: Lens[S, A])(snd: Optional[A, B]): Optional[S, B] =
        Optional[S, B](fst.get _ andThen snd.getOption)(b => s => fst.set(snd.set(b)(fst.get(s)))(s))
    }

    trait PPrism[S, T, A, B] {
      def getOption(s: S): Option[A]

      // Capable of construct a T only from B
      def reverseGet(b: B): T
    }

    type Prism[S, A] = PPrism[S, S, A, A]

    object Prism {

      def apply[S, A](_getOption: S => Option[A])(_reverseGet: A => S) =
        new PPrism[S, S, A, A] {
          def getOption(s: S): Option[A] = _getOption(s)
          def reverseGet(a: A): S = _reverseGet(a)
        }

      def partial[S, A](get: PartialFunction[S, A])(reverseGet: A => S) =
        Prism[S, A](get.lift)(reverseGet)

      def asOptional[S, A](p: Prism[S, A]): Optional[S, A] =
        Optional[S, A](p.getOption)(a => _ => p.reverseGet(a))

      def composePrism[S, A, B](fst: Prism[S, A])(snd: Prism[A, B]): Prism[S, B] =
        Prism[S, B](fst.getOption(_) flatMap snd.getOption)(b => fst.reverseGet(snd.reverseGet(b)))

      def composeLens[S, A, B](fst: Prism[S, A])(snd: Lens[A, B]): Optional[S, B] =
        Optional[S, B](fst.getOption(_) flatMap (snd.get _ andThen Option.apply))(b => s => fst.getOption(s).fold(s)(a => fst.reverseGet(snd.set(b)(a))))

      def composeOptional[S, A, B](fst: Prism[S, A])(snd: Optional[A, B]): Optional[S, B] =
        Optional.composeOptional(asOptional(fst))(snd)
    }

    trait POptional[S, T, A, B] {
      def getOption(s: S): Option[A]
      def set(b: B)(s: S): T
    }

    type Optional[S, A] = POptional[S, S, A, A]

    object Optional {
      def apply[S, A](_get: S => Option[A])(_set: A => S => S) =
        new Optional[S, A] {
          def getOption(s: S): Option[A] = _get(s)
          def set(a: A)(s: S): S = _set(a)(s)
        }

      def composeOptional[S, A, B](fst: Optional[S, A])(snd: Optional[A, B]): Optional[S, B] =
        Optional[S, B](fst.getOption(_) flatMap snd.getOption)(b => s => fst.getOption(s).fold(s)(a => fst.set(snd.set(b)(a))(s)))

      def composeLens[S, A, B](fst: Optional[S, A])(snd: Lens[A, B]): Optional[S, B] =
        composeOptional(fst)(Lens.asOptional(snd))

      def composePrism[S, A, B](fst: Optional[S, A])(snd: Prism[A, B]): Optional[S, B] =
        composeOptional(fst)(Prism.asOptional(snd))
    }
  }
}

object OpticsPlay extends App {
  // References
  // https://blog.scalac.io/optics-beyond-lenses-with-monocle.html

  object cases {

    object events {
      type Id = Int
      type Ip = String
      type Message = String

      // sealed abstract class Event(_id: Id, _msg: Message) {
      //   def id: Id = this._id
      //   def msg: Message = this._msg
      // }
      sealed trait Event {
        val id: Id
        val msg: Message
      }

      object Event {
        final case class ClientEvent(id: Id, msg: Message, client: Client) extends Event
        final case class ServerEvent(id: Id, msg: Message, machine: Server) extends Event
      }

      final case class Client(name: String, provider: String)
      final case class Server(name: String, ip: Ip, location: String)

      object optics {
        import Event._
        import playground.optics._
        import polymorphic._
        //import monomorphic._

        val eventMsgL = Lens[Event, Message](_.msg) {
          a =>
            _ match {
              case ce @ ClientEvent(i, m, c) => ce.copy(msg = a)
              case se @ ServerEvent(i, m, s) => se.copy(msg = a)
            }
        }

        val eventServerO = Optional[Event, Server] {
          _ match {
            case se @ ServerEvent(i, m, s) => Some(s)
            case _ => None
          }
        } {
          a =>
            _ match {
              case ce @ ClientEvent(i, m, c) => ce
              case se @ ServerEvent(i, m, s) => se.copy(machine = a)
            }
        }

        val serverIpL = Lens[Server, Ip](_.ip) {
          a => _.copy(ip = a)
        }
      }
    }

    object json {

      sealed trait Json

      object Json {
        final case object JNull extends Json
        final case class JStr(v: String) extends Json
        final case class JDouble(v: Double) extends Json
        final case class JObj(v: Map[String, Json]) extends Json
      }

      // Exercices

    }
  }

  {
    import cases.events._, Event._

    val c1 = Client("Client1", "Provider1")
    val e1 = ClientEvent(1, "client event msg", c1)
    val s1 = Server("Server1", "1.1.1.1", "Europe")
    val e2 = ServerEvent(2, "server event msg", s1)

    import cases.events.optics._

    println(s"""${eventMsgL.get(e1)}""")
    println(s"""${eventMsgL.get(e2)}""")
    println(s"""${serverIpL.get(s1)}""")
    println(s"""${serverIpL.set("2.2.2.2")(s1)}""")

    import playground.optics.polymorphic._

    val eventIpO = Optional.composeLens(eventServerO)(serverIpL)

    println(s"""${eventIpO.getOption(e1)}""")
    println(s"""${eventIpO.getOption(e2)}""")
  }
}

/*
 Define Traversal, Fold, Index,...
 */
