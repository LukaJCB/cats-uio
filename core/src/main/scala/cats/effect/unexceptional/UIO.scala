package cats.effect.unexceptional

import cats.{Applicative, Monad, MonadError, Monoid, Parallel, Semigroup, ~>}
import cats.effect.{Fiber, IO, Timer}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration


private[unexceptional] trait Newtype { self =>
  private[unexceptional] type Base
  private[unexceptional] trait Tag extends Any
  type Type[A] <: Base with Tag
}

object UIOImpl extends UIOInstances with Newtype {
  private[cats] def create[A](s: IO[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  def fromIO[A](ioa: IO[A]): UIO[Either[Throwable, A]] =
    create(ioa.attempt)

  def runUIO[A](uioa: UIO[A]): IO[A] =
    uioa.asInstanceOf[IO[A]]

  def runEitherIO[A](uioa: UIO[Either[Throwable, A]]): IO[A] =
    MonadError[IO, Throwable].rethrow(runUIO(uioa))

  def unsafeFromIO[A](ioa: IO[A]): UIO[A] = create(ioa)

  def pure[A](x: A): UIO[A] = create(IO.pure(x))

  def apply[A](x: => A): UIO[Either[Throwable, A]] =
    fromIO(IO(x))

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): UIO[Either[Throwable, A]] =
    fromIO(IO.async(k))

  def suspend[A](thunk: => UIO[A]): UIO[A] =
    unsafeFromIO(IO.suspend(runUIO(thunk)))

  val unit: UIO[Unit] =
    pure(())

  def race[A, B](lh: UIO[A], rh: UIO[B]): UIO[Either[A, B]] =
    unsafeFromIO(IO.race(runUIO(lh), runUIO(rh)))


  def cancelable[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): UIO[Either[Throwable, A]] =
     fromIO(IO.cancelable(k))


  def start[A](uioa: UIO[A]): UIO[Fiber[UIO, A]] =
    unsafeFromIO(runUIO(uioa).start.map(uioFiber))


  def fromFuture[A](iof: UIO[Future[A]]): UIO[Either[Throwable, A]] =
    fromIO(IO.fromFuture(runUIO(iof)))

  def shift(implicit timer: Timer[UIO]): UIO[Unit] =
    timer.shift

  def sleep(duration: FiniteDuration)(implicit timer: Timer[UIO]): UIO[Unit] =
    timer.sleep(duration)

  val cancelBoundary: UIO[Unit] = unsafeFromIO(IO.cancelBoundary)

  def racePair[A, B](lh: UIO[A], rh: UIO[B]): UIO[Either[(A, Fiber[UIO, B]), (Fiber[UIO, A], B)]] = {
    import cats.syntax.bifunctor._
    import cats.instances.either._

    UIOImpl.unsafeFromIO(IO.racePair(runUIO(lh), runUIO(rh)).map { e =>
      e.bimap({
        case (a, fiber) => (a, uioFiber(fiber))
      }, {
        case (fiber, b) => (uioFiber(fiber), b)
      })
    })
  }

  private def uioFiber[A](f: Fiber[IO, A]): Fiber[UIO, A] =
    Fiber(unsafeFromIO(f.join), unsafeFromIO(f.cancel))

}

private[unexceptional] abstract class UIOParallelNewtype {

  type Par[A] = Par.Type[A]

  object Par extends Newtype {

    def fromUIO[A](s: UIO[A]): Type[A] =
      s.asInstanceOf[Type[A]]

    def toUIO[A](s: Type[A]): UIO[A] =
      s.asInstanceOf[UIO[A]]

  }
}

private[unexceptional] sealed abstract class UIOInstances extends UIOParallelNewtype {
  implicit val catsEffectMonadForUIO: Monad[UIO] = new Monad[UIO] {
    def tailRecM[A, B](a: A)(f: A => UIO[Either[A, B]]): UIO[B] =
      UIOImpl.create(Monad[IO].tailRecM(a)(f andThen UIOImpl.runUIO))

    def flatMap[A, B](fa: UIO[A])(f: A => UIO[B]): UIO[B] =
      UIOImpl.create(Monad[IO].flatMap(UIOImpl.runUIO(fa))(f andThen UIOImpl.runUIO))

    def pure[A](x: A): UIO[A] =
      UIOImpl.pure(x)
  }

  implicit val catsEffectApplicativeForParUIO: Applicative[UIOImpl.Par] = new Applicative[UIOImpl.Par] {
    def pure[A](x: A): UIOImpl.Par[A] = UIOImpl.Par.fromUIO(UIOImpl.pure(x))

    def ap[A, B](ff: UIOImpl.Par[A => B])(fa: UIOImpl.Par[A]): UIOImpl.Par[B] =
      UIOImpl.Par.fromUIO(UIOImpl.create(Parallel.parAp(UIOImpl.runUIO(UIOImpl.Par.toUIO(ff)))(UIOImpl.runUIO(UIOImpl.Par.toUIO(fa)))))
  }

  implicit val catsEffectParallelForUIO: Parallel[UIO, UIOImpl.Par] = new Parallel[UIO, UIOImpl.Par] {
    def applicative: Applicative[UIOImpl.Par] = catsEffectApplicativeForParUIO

    def monad: Monad[UIO] = catsEffectMonadForUIO

    def sequential: ~>[UIOImpl.Par, UIO] =
      new ~>[UIOImpl.Par, UIO] { def apply[A](fa: UIOImpl.Par[A]): UIO[A] = UIOImpl.Par.toUIO(fa) }

    def parallel: ~>[UIO, UIOImpl.Par] =
      new ~>[UIO, UIOImpl.Par] { def apply[A](fa: UIO[A]): UIOImpl.Par[A] = UIOImpl.Par.fromUIO(fa) }
  }

  implicit def catsEffectMonoidForUIO[A: Monoid]: Monoid[UIO[A]] = new Monoid[UIO[A]] {
    def empty: UIO[A] = UIOImpl.pure(Monoid[A].empty)
    def combine(x: UIO[A], y: UIO[A]): UIO[A] =
      UIOImpl.create(UIOImpl.runUIO(x).flatMap(a1 => UIOImpl.runUIO(y).map(a2 => Semigroup[A].combine(a1, a2))))
  }
}
