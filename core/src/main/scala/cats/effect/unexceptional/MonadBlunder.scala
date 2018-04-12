package cats.effect.unexceptional

import cats.data.EitherT
import cats.{Applicative, Monad, MonadError}
import cats.effect.IO

trait MonadBlunder[F[_], G[_], E] {
  def monadErrorF: MonadError[F, E]
  def applicativeG: Applicative[G]

  def handleBlunderWith[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def endeavor[A](fa: F[A]): G[Either[E, A]] =
    handleBlunderWith(monadErrorF.map(fa)(Right(_): Either[E, A]))(e => applicativeG.pure(Left(e)))

  def endeavorT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(endeavor(fa))


  def handleBlunder[A](fa: F[A])(f: E => A): G[A] =
    handleBlunderWith(fa)(f andThen applicativeG.pure)


  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.rethrow(accept(gea))

  def assure[A](ga: G[A])(error: => E)(predicate: A => Boolean): F[A] =
    assureOr(ga)(_ => error)(predicate)

  def assureOr[A](ga: G[A])(error: A => E)(predicate: A => Boolean): F[A] =
    monadErrorF.flatMap(accept(ga))(a =>
      if (predicate(a)) monadErrorF.pure(a) else monadErrorF.raiseError(error(a)))

}

object MonadBlunder {
  implicit val catsEndeavorForIO: MonadBlunder[IO, UIO, Throwable] = new MonadBlunder[IO, UIO, Throwable] {
    override def monadErrorF: MonadError[IO, Throwable] = cats.effect.IO.ioConcurrentEffect
    override def applicativeG: Monad[UIO] = cats.effect.unexceptional.UIO.catsEffectMonadForUIO

    override def endeavor[A](fa: IO[A]): UIO[Either[Throwable, A]] = UIO.fromIO(fa)

    override def handleBlunderWith[A](fa: IO[A])(f: Throwable => UIO[A]): UIO[A] =
      UIO.unsafeFromIO(monadErrorF.handleErrorWith(fa)(f andThen accept))

    override def accept[A](ga: UIO[A]): IO[A] = UIO.runUIO(ga)
  }
}
