//> using scala "3.7.1"
//> using dep   "org.typelevel::cats-effect::3.7.0-RC1"
//> using options "-deprecation" "-Wunused:all"

import cats.effect.{IO, IOApp}
import cats.effect.std.Queue
import cats.syntax.all._

object QueueBench extends IOApp.Simple {

  /** messages to shuttle (env SIZE, default 1 000 000) */
  private val size: Int =
    sys.env.getOrElse("SIZE", "1000000").toInt

  /** queue capacity (env CAP, default 1024) */
  private val cap: Int =
    sys.env.getOrElse("CAP", "1024").toInt

  // tail-recursive producer
  def produce(q: Queue[IO, Int], n: Int, i: Int = 0): IO[Unit] =
    if i < n then q.offer(i) >> produce(q, n, i + 1) else IO.unit

  // tail-recursive consumer
  def consume(q: Queue[IO, Int], n: Int, soFar: Int = 0): IO[Unit] =
    if soFar < n then q.take >> consume(q, n, soFar + 1) else IO.unit

  /** concurrent run: producer ∥ consumer */
  def parTransfer(size: Int, cap: Int): IO[Unit] =
    for
      q <- Queue.bounded[IO, Int](cap)
      _ <- (produce(q, size), consume(q, size)).parTupled.void.void
    yield ()

  def timed[A](ioa: IO[A]): IO[Long] =
    for
      t0 <- IO.monotonic
      _ <- ioa
      t1 <- IO.monotonic
    yield (t1 - t0).toMillis

  val run: IO[Unit] =
    for
      _ <- IO.println(s"size: $size, cap: $cap")
      // warmup
      _ <- IO.println("warmup: 100000 msgs, 1024 queue cap")
      warmupMs <- timed(parTransfer(100000, 1024))
      _ <- IO.println(s"warmup: $warmupMs ms")
      _ <- IO.println(s"benchmark: $size msgs, $cap queue cap")
      parMs <- timed(parTransfer(size, cap))
      _ <- IO.println(s"concurrent producer→consumer ($size msgs, $cap queue cap): $parMs ms")
    yield ()
}
