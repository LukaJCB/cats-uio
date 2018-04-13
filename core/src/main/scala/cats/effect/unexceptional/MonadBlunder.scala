package cats.effect.unexceptional

import cats.data.EitherT
import cats.{Applicative, Id, MonadError, Eq}
import cats.effect.IO
import cats.syntax.all._

import scala.concurrent.{ExecutionContext, Future}

trait MonadBlunder[F[_], G[_], E] {
  def monadErrorF: MonadError[F, E]
  def applicativeG: Applicative[G]

  def handleBlunderWith[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def endeavor[A](fa: F[A]): G[Either[E, A]] =
    handleBlunder(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

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

  trait MonadBlunderLaws[F[_], G[_], E, A] {
    def deriveHandleError(fa: F[A])
                         (f: E => A)
                         (implicit M: MonadBlunder[F, G, E], E: Eq[F[A]]): Boolean =
      M.accept(M.handleBlunder(fa)(f)) === M.monadErrorF.handleError(fa)(f)

    def deriveAttempt(fa: F[A])(implicit M: MonadBlunder[F, G, E], E: Eq[F[Either[E, A]]]): Boolean =
      M.accept(M.endeavor(fa)) === M.monadErrorF.attempt(fa)

    def deriveEnsureOr(ga: G[A])
                      (error: A => E)
                      (predicate: A => Boolean)
                      (implicit M: MonadBlunder[F, G, E], E: Eq[F[A]]): Boolean =
      M.monadErrorF.ensureOr(M.accept(ga))(error)(predicate) === M.assureOr(ga)(error)(predicate)

    def raiseErrorHandleBlunderWith(e: E, f: E => G[A])
                                   (implicit M: MonadBlunder[F, G, E], E: Eq[G[A]]): Boolean =
      M.handleBlunderWith(M.monadErrorF.raiseError[A](e))(f) === f(e)


    def raiseErrorHandleBlunder(e: E, f: E => A)
                               (implicit M: MonadBlunder[F, G, E], E: Eq[G[A]]): Boolean =
      M.handleBlunder(M.monadErrorF.raiseError[A](e))(f) === M.applicativeG.pure(f(e))


    def raiseErrorEndeavor(e: E)
                          (implicit M: MonadBlunder[F, G, E], E: Eq[G[Either[E, A]]]): Boolean =
      M.endeavor(M.monadErrorF.raiseError[A](e)) === M.applicativeG.pure(Left(e))

    def endeavorAbsolve(fa: F[A])(implicit M: MonadBlunder[F, G, E], E: Eq[F[A]]): Boolean =
      M.absolve(M.endeavor(fa)) === fa
  }


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

  implicit def catsEndeavorForOption: MonadBlunder[Option, Id, Unit] =
    new MonadBlunder[Option, Id, Unit] {
      def monadErrorF: MonadError[Option, Unit] = cats.instances.option.catsStdInstancesForOption
      def applicativeG: Applicative[Id] = cats.catsInstancesForId

      def handleBlunderWith[A](fa: Option[A])(f: Unit => A): A = fa match {
        case Some(a) => a
        case None => f(())
      }

      def accept[A](ga: A): Option[A] = Some(ga)
    }
}
