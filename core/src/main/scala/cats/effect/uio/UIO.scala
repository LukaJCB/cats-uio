package cats.effect.uio

import cats.{Applicative, Monad, MonadError, Monoid, Parallel, Semigroup, ~>}
import cats.effect.IO


private[uio] trait Newtype { self =>
  private[uio] type Base
  private[uio] trait Tag extends Any
  type Type[A] <: Base with Tag
}

object UIO extends UIOInstances with Newtype {
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
}

private[uio] abstract class UIOParallelNewtype {

  type Par[A] = Par.Type[A]

  object Par extends Newtype {

    def fromUIO[A](s: UIO[A]): Type[A] =
      s.asInstanceOf[Type[A]]

    def toUIO[A](s: Type[A]): UIO[A] =
      s.asInstanceOf[UIO[A]]

  }
}

private[uio] sealed abstract class UIOInstances extends UIOParallelNewtype {
  implicit val catsEffectMonadForUIO: Monad[UIO] = new Monad[UIO] {
    def tailRecM[A, B](a: A)(f: A => UIO[Either[A, B]]): UIO[B] =
      UIO.create(Monad[IO].tailRecM(a)(f andThen UIO.runUIO))

    def flatMap[A, B](fa: UIO[A])(f: A => UIO[B]): UIO[B] =
      UIO.create(Monad[IO].flatMap(UIO.runUIO(fa))(f andThen UIO.runUIO))

    def pure[A](x: A): UIO[A] =
      UIO.pure(x)
  }

  implicit val catsEffectApplicativeForParUIO: Applicative[UIO.Par] = new Applicative[UIO.Par] {
    def pure[A](x: A): UIO.Par[A] = UIO.Par.fromUIO(UIO.pure(x))

    def ap[A, B](ff: UIO.Par[A => B])(fa: UIO.Par[A]): UIO.Par[B] =
      UIO.Par.fromUIO(UIO.create(Parallel.parAp(UIO.runUIO(UIO.Par.toUIO(ff)))(UIO.runUIO(UIO.Par.toUIO(fa)))))
  }

  implicit val catsEffectParallelForUIO: Parallel[UIO, UIO.Par] = new Parallel[UIO, UIO.Par] {
    def applicative: Applicative[UIO.Par] = catsEffectApplicativeForParUIO

    def monad: Monad[UIO] = catsEffectMonadForUIO

    def sequential: ~>[UIO.Par, UIO] =
      new ~>[UIO.Par, UIO] { def apply[A](fa: UIO.Par[A]): UIO[A] = UIO.Par.toUIO(fa) }

    def parallel: ~>[UIO, UIO.Par] =
      new ~>[UIO, UIO.Par] { def apply[A](fa: UIO[A]): UIO.Par[A] = UIO.Par.fromUIO(fa) }
  }

  implicit def catsEffectMonoidForUIO[A: Monoid]: Monoid[UIO[A]] = new Monoid[UIO[A]] {
    def empty: UIO[A] = UIO.pure(Monoid[A].empty)
    def combine(x: UIO[A], y: UIO[A]): UIO[A] =
      UIO.create(UIO.runUIO(x).flatMap(a1 => UIO.runUIO(y).map(a2 => Semigroup[A].combine(a1, a2))))
  }
}
