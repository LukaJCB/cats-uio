package cats.effect.unexceptional

import cats.data.{EitherT, IndexedStateT, OptionT, StateT}
import cats.{Eq, Id, Monad, MonadError, ~>}
import cats.effect.IO
import cats.syntax.all._

import scala.concurrent.{ExecutionContext, Future}

trait MonadBlunder[F[_], G[_], E] {
  val monadErrorF: MonadError[F, E]
  val monadG: Monad[G]

  def handleBlunderWith[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def endeavor[A](fa: F[A]): G[Either[E, A]] =
    handleBlunder(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

  def endeavorT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(endeavor(fa))

  def handleBlunder[A](fa: F[A])(f: E => A): G[A] =
    handleBlunderWith(fa)(f andThen monadG.pure)

  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.flatMap(accept(gea))(_.fold(monadErrorF.raiseError, monadErrorF.pure))

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

    def bindAlwaysWorksInG(ga: G[A], a: A)(implicit M: MonadBlunder[F, G, E], E: Eq[G[A]]): Boolean =
      M.monadG.flatMap(ga)(_ => M.monadG.pure(a)) === M.monadG.pure(a)

    def raiseErrorHandleBlunderWith(e: E, f: E => G[A])
                                   (implicit M: MonadBlunder[F, G, E], E: Eq[G[A]]): Boolean =
      M.handleBlunderWith(M.monadErrorF.raiseError[A](e))(f) === f(e)


    def raiseErrorHandleBlunder(e: E, f: E => A)
                               (implicit M: MonadBlunder[F, G, E], E: Eq[G[A]]): Boolean =
      M.handleBlunder(M.monadErrorF.raiseError[A](e))(f) === M.monadG.pure(f(e))


    def raiseErrorEndeavor(e: E)
                          (implicit M: MonadBlunder[F, G, E], E: Eq[G[Either[E, A]]]): Boolean =
      M.endeavor(M.monadErrorF.raiseError[A](e)) === M.monadG.pure(Left(e))

    def endeavorAbsolve(fa: F[A])(implicit M: MonadBlunder[F, G, E], E: Eq[F[A]]): Boolean =
      M.absolve(M.endeavor(fa)) === fa
  }


  implicit val catsEndeavorForIO: MonadBlunder[IO, UIO, Throwable] = new MonadBlunder[IO, UIO, Throwable] {
    val monadErrorF: MonadError[IO, Throwable] = cats.effect.IO.ioConcurrentEffect
    val monadG: Monad[UIO] = cats.effect.unexceptional.UIO.catsEffectMonadForUIO

    override def endeavor[A](fa: IO[A]): UIO[Either[Throwable, A]] = UIO.fromIO(fa)

    def handleBlunderWith[A](fa: IO[A])(f: Throwable => UIO[A]): UIO[A] =
      UIO.unsafeFromIO(monadErrorF.handleErrorWith(fa)(f andThen accept))

    def accept[A](ga: UIO[A]): IO[A] = UIO.runUIO(ga)
  }

  implicit def catsEndeavorForFuture(implicit ev: ExecutionContext): MonadBlunder[Future, Unexceptional[Future, ?], Throwable] =
    new MonadBlunder[Future, Unexceptional[Future, ?], Throwable] {

      import cats.instances.future._

      val monadErrorF: MonadError[Future, Throwable] = catsStdInstancesForFuture
      val monadG: Monad[Unexceptional[Future, ?]] =
        Unexceptional.catsEffectMonadForUnexceptional[Future]

      def handleBlunderWith[A](fa: Future[A])(f: Throwable => Unexceptional[Future, A]): Unexceptional[Future, A] =
        Unexceptional.unsafeFromF(fa.recoverWith(PartialFunction(f andThen Unexceptional.runUnexceptional)))

      override def accept[A](ga: Unexceptional[Future, A]): Future[A] =
        Unexceptional.runUnexceptional(ga)
    }

  implicit def catsEndeavorForEither[E]: MonadBlunder[Either[E, ?], Id, E] =
    new MonadBlunder[Either[E, ?], Id, E] {
      val monadErrorF: MonadError[Either[E, ?], E] = cats.instances.either.catsStdInstancesForEither
      val monadG: Monad[Id] = cats.catsInstancesForId

      def handleBlunderWith[A](fa: Either[E, A])(f: E => A): A = fa match {
        case Left(e) => f(e)
        case Right(a) => a
      }

      def accept[A](ga: A): Either[E, A] = Right(ga)
    }

  implicit def catsEndeavorForEitherT[F[_]: Monad, E]: MonadBlunder[EitherT[F, E, ?], F, E] =
    new MonadBlunder[EitherT[F, E, ?], F, E] {
      val monadErrorF: MonadError[EitherT[F, E, ?], E] = EitherT.catsDataMonadErrorForEitherT
      val monadG: Monad[F] = Monad[F]

      def handleBlunderWith[A](fa: EitherT[F, E, A])(f: E => F[A]): F[A] =
        fa.value.flatMap {
          case Left(e) => f(e)
          case Right(a) => monadG.pure(a)
        }

      def accept[A](ga: F[A]): EitherT[F, E, A] =
        EitherT.liftF(ga)

    }

  implicit def catsEndeavorForOption: MonadBlunder[Option, Id, Unit] =
    new MonadBlunder[Option, Id, Unit] {
      val monadErrorF: MonadError[Option, Unit] = cats.instances.option.catsStdInstancesForOption
      val monadG: Monad[Id] = cats.catsInstancesForId

      def handleBlunderWith[A](fa: Option[A])(f: Unit => A): A = fa match {
        case Some(a) => a
        case None => f(())
      }

      def accept[A](ga: A): Option[A] = Some(ga)
    }

  implicit def catsEndeavorForOptionT[F[_]: Monad, E]: MonadBlunder[OptionT[F, ?], F, Unit] =
    new MonadBlunder[OptionT[F, ?], F, Unit] {
      val monadErrorF: MonadError[OptionT[F, ?], Unit] = new OptionTMonadError[F] {def F: Monad[F] = Monad[F]}
      val monadG: Monad[F] = Monad[F]

      def handleBlunderWith[A](fa: OptionT[F, A])(f: Unit => F[A]): F[A] =
        fa.value.flatMap {
          case Some(a) => monadG.pure(a)
          case None => f(())
        }

      def accept[A](ga: F[A]): OptionT[F, A] =
        OptionT.liftF(ga)

    }

  implicit def catsEndeavorForStateT[F[_], G[_], S, E]
    (implicit M: MonadBlunder[F, G, E]): MonadBlunder[StateT[F, S, ?], StateT[G, S, ?], E] =
    new MonadBlunder[StateT[F, S, ?], StateT[G, S, ?], E] {
      implicit val F: MonadError[F, E] = M.monadErrorF
      implicit val G: Monad[G] = M.monadG

      val monadErrorF: MonadError[StateT[F, S, ?], E] = IndexedStateT.catsDataMonadErrorForIndexedStateT
      val monadG: Monad[StateT[G, S, ?]] = IndexedStateT.catsDataMonadForIndexedStateT

      def accept[A](ga: StateT[G, S, A]): StateT[F, S, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = M.accept(ga)
      })

      def handleBlunderWith[A](fa: StateT[F, S, A])(f: E => StateT[G, S, A]): StateT[G, S, A] =
        IndexedStateT(s => M.handleBlunderWith(fa.run(s))(e => f(e).run(s)))

    }
}

private[effect] trait OptionTMonadError[F[_]] extends MonadError[OptionT[F, ?], Unit] {
  implicit def F: Monad[F]

  def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa.flatMap(f)

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)

  def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
    OptionT(F.tailRecM(a)(a0 => F.map(f(a0).value)(
      _.fold(Either.right[A, Option[B]](None))(_.map(b => Some(b): Option[B]))
    )))

  def raiseError[A](e: Unit): OptionT[F, A] = OptionT.none

  def handleErrorWith[A](fa: OptionT[F, A])(f: Unit => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.flatMap(fa.value) {
      case s @ Some(_) => F.pure(s)
      case None => f(()).value
    })
}
