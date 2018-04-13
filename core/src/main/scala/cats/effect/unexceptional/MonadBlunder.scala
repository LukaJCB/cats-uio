package cats.effect.unexceptional

import cats.data.EitherT
import cats.{Applicative, Id, MonadError}
import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}

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
    def monadErrorF: MonadError[IO, Throwable] = cats.effect.IO.ioConcurrentEffect
    def applicativeG: Applicative[UIO] = cats.effect.unexceptional.UIO.catsEffectMonadForUIO

    override def endeavor[A](fa: IO[A]): UIO[Either[Throwable, A]] = UIO.fromIO(fa)

    def handleBlunderWith[A](fa: IO[A])(f: Throwable => UIO[A]): UIO[A] =
      UIO.unsafeFromIO(monadErrorF.handleErrorWith(fa)(f andThen accept))

    def accept[A](ga: UIO[A]): IO[A] = UIO.runUIO(ga)
  }

  implicit def catsEndeavorForFuture(implicit ev: ExecutionContext): MonadBlunder[Future, Unexceptional[Future, ?], Throwable] =
    new MonadBlunder[Future, Unexceptional[Future, ?], Throwable] {

      import cats.instances.future._

      def monadErrorF: MonadError[Future, Throwable] = catsStdInstancesForFuture
      def applicativeG: Applicative[Unexceptional[Future, ?]] =
        Unexceptional.catsEffectApplicativeForUnexceptional[Future]

      def handleBlunderWith[A](fa: Future[A])(f: Throwable => Unexceptional[Future, A]): Unexceptional[Future, A] =
        Unexceptional.unsafeFromF(fa.recoverWith(PartialFunction(f andThen Unexceptional.runUnexceptional)))

      override def accept[A](ga: Unexceptional[Future, A]): Future[A] =
        Unexceptional.runUnexceptional(ga)
    }

  implicit def catsEndeavorForEither[E]: MonadBlunder[Either[E, ?], Id, E] =
    new MonadBlunder[Either[E, ?], Id, E] {
      def monadErrorF: MonadError[Either[E, ?], E] = cats.instances.either.catsStdInstancesForEither
      def applicativeG: Applicative[Id] = cats.catsInstancesForId

      def handleBlunderWith[A](fa: Either[E, A])(f: E => A): A = fa match {
        case Left(e) => f(e)
        case Right(a) => a
      }

      def accept[A](ga: A): Either[E, A] = Right(ga)
    }
}
