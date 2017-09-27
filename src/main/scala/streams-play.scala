package playground

object FS2Play extends App {
  import fs2.Stream._


}

object StreamPlay extends App {
  type Stream[F[_], O] = () => F[O]
  type Pipe[F[_], I, O] = Stream[F, I] => Stream[F, O]
  type Sink[F[_], I] = Pipe[F, I, Unit]
}
