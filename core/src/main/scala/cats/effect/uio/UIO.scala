package cats.effect.uio

import cats.{Monad, MonadError, Monoid, Semigroup}
import cats.effect.IO


private[uio] trait Newtype { self =>
  private[uio] type Base
  private[uio] trait Tag extends Any
  type Type[A] <: Base with Tag
}

object UIO extends UIOInstances with Newtype {
  private[cats] def create[A](s: IO[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[cats] def unwrap[A](s: Type[A]): IO[A] =
    s.asInstanceOf[IO[A]]

  def fromIO[A](ioa: IO[A]): UIO[Either[Throwable, A]] =
    create(ioa.attempt)

  def runUIO[A](uioa: UIO[A]): IO[A] = unwrap(uioa)

  def runEitherIO[A](uioa: UIO[Either[Throwable, A]]): IO[A] =
    MonadError[IO, Throwable].rethrow(runUIO(uioa))

  def unsafeFromIO[A](ioa: IO[A]): UIO[A] = create(ioa)

  def pure[A](x: A): UIO[A] = create(IO.pure(x))
}

private[uio] sealed abstract class UIOInstances {
  implicit def catsEffectMonadForUIO: Monad[UIO] = new Monad[UIO] {
    def tailRecM[A, B](a: A)(f: A => UIO[Either[A, B]]): UIO[B] =
      UIO.create(Monad[IO].tailRecM(a)(f andThen UIO.unwrap))

    def flatMap[A, B](fa: UIO[A])(f: A => UIO[B]): UIO[B] =
      UIO.create(Monad[IO].flatMap(UIO.unwrap(fa))(f andThen UIO.unwrap))

    def pure[A](x: A): UIO[A] =
      UIO.pure(x)
  }

  implicit def catsEffectMonoidForUIO[A: Monoid]: Monoid[UIO[A]] = new Monoid[UIO[A]] {
    def empty: UIO[A] = UIO.pure(Monoid[A].empty)
    def combine(x: UIO[A], y: UIO[A]): UIO[A] =
      UIO.create(UIO.unwrap(x).flatMap(a1 => UIO.unwrap(y).map(a2 => Semigroup[A].combine(a1, a2))))
  }
}
