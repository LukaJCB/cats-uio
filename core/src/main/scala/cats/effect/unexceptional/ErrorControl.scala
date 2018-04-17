package cats.effect.unexceptional

import cats.data._
import cats.{Applicative, Eq, Id, Monad, MonadError, Monoid, ~>}
import cats.effect.IO
import cats.syntax.all._

import scala.concurrent.{ExecutionContext, Future}

trait ErrorControl[F[_], G[_], E] {
  val monadErrorF: MonadError[F, E]
  val applicativeG: Applicative[G]

  def controlError[A](fa: F[A])(f: E => G[A]): G[A]

  def accept[A](ga: G[A]): F[A]

  def trial[A](fa: F[A]): G[Either[E, A]] =
    intercept(monadErrorF.map(fa)(Right(_): Either[E, A]))(Left(_))

  def trialT[A](fa: F[A]): EitherT[G, E, A] =
    EitherT(trial(fa))

  def intercept[A](fa: F[A])(f: E => A): G[A] =
    controlError(fa)(f andThen applicativeG.pure)

  def absolve[A](gea: G[Either[E, A]]): F[A] =
    monadErrorF.flatMap(accept(gea))(_.fold(monadErrorF.raiseError, monadErrorF.pure))

  def assure[A](ga: G[A])(error: => E)(predicate: A => Boolean): F[A] =
    assureOr(ga)(_ => error)(predicate)

  def assureOr[A](ga: G[A])(error: A => E)(predicate: A => Boolean): F[A] =
    monadErrorF.flatMap(accept(ga))(a =>
      if (predicate(a)) monadErrorF.pure(a) else monadErrorF.raiseError(error(a)))

}

object ErrorControl {

  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControl[F, G, E] = ev

  trait MonadBlunderLaws[F[_], G[_], E, A] {
    def deriveHandleError(fa: F[A])
                         (f: E => A)
                         (implicit M: ErrorControl[F, G, E], E: Eq[F[A]]): Boolean =
      M.accept(M.intercept(fa)(f)) === M.monadErrorF.handleError(fa)(f)

    def deriveAttempt(fa: F[A])(implicit M: ErrorControl[F, G, E], E: Eq[F[Either[E, A]]]): Boolean =
      M.accept(M.trial(fa)) === M.monadErrorF.attempt(fa)

    def deriveEnsureOr(ga: G[A])
                      (error: A => E)
                      (predicate: A => Boolean)
                      (implicit M: ErrorControl[F, G, E], E: Eq[F[A]]): Boolean =
      M.monadErrorF.ensureOr(M.accept(ga))(error)(predicate) === M.assureOr(ga)(error)(predicate)

    def gNeverHasErrors(ga: G[A], f: E => A)(implicit M: ErrorControl[F, G, E], E: Eq[G[A]]): Boolean =
      M.intercept(M.accept(ga))(f) === ga

    def raiseErrorHandleBlunderWith(e: E, f: E => G[A])
                                   (implicit M: ErrorControl[F, G, E], E: Eq[G[A]]): Boolean =
      M.controlError(M.monadErrorF.raiseError[A](e))(f) === f(e)


    def handleErrorPureIsPure(a: A, f: E => G[A])(implicit M: ErrorControl[F, G, E], E: Eq[G[A]]): Boolean = {
      M.controlError(M.monadErrorF.pure(a))(f) === M.applicativeG.pure(a)
    }


    def raiseErrorHandleBlunder(e: E, f: E => A)
                               (implicit M: ErrorControl[F, G, E], E: Eq[G[A]]): Boolean =
      M.intercept(M.monadErrorF.raiseError[A](e))(f) === M.applicativeG.pure(f(e))


    def raiseErrorEndeavor(e: E)
                          (implicit M: ErrorControl[F, G, E], E: Eq[G[Either[E, A]]]): Boolean =
      M.trial(M.monadErrorF.raiseError[A](e)) === M.applicativeG.pure(Left(e))

    def endeavorAbsolve(fa: F[A])(implicit M: ErrorControl[F, G, E], E: Eq[F[A]]): Boolean =
      M.absolve(M.trial(fa)) === fa
  }


  implicit val catsEndeavorForIO: ErrorControl[IO, UIO, Throwable] = new ErrorControl[IO, UIO, Throwable] {
    val monadErrorF: MonadError[IO, Throwable] = cats.effect.IO.ioConcurrentEffect
    val applicativeG: Monad[UIO] = cats.effect.unexceptional.UIO.catsEffectMonadForUIO

    override def trial[A](fa: IO[A]): UIO[Either[Throwable, A]] = UIO.fromIO(fa)

    def controlError[A](fa: IO[A])(f: Throwable => UIO[A]): UIO[A] =
      UIO.unsafeFromIO(monadErrorF.handleErrorWith(fa)(f andThen accept))

    def accept[A](ga: UIO[A]): IO[A] = UIO.runUIO(ga)
  }

  implicit def catsEndeavorForFuture(implicit ev: ExecutionContext): ErrorControl[Future, Unexceptional[Future, ?], Throwable] =
    new ErrorControl[Future, Unexceptional[Future, ?], Throwable] {

      import cats.instances.future._

      val monadErrorF: MonadError[Future, Throwable] = catsStdInstancesForFuture
      val applicativeG: Applicative[Unexceptional[Future, ?]] =
        Unexceptional.catsEffectMonadForUnexceptional[Future]

      def controlError[A](fa: Future[A])(f: Throwable => Unexceptional[Future, A]): Unexceptional[Future, A] =
        Unexceptional.unsafeFromF(fa.recoverWith(PartialFunction(f andThen Unexceptional.runUnexceptional)))

      override def accept[A](ga: Unexceptional[Future, A]): Future[A] =
        Unexceptional.runUnexceptional(ga)
    }

  implicit def catsEndeavorForEither[E]: ErrorControl[Either[E, ?], Id, E] =
    new ErrorControl[Either[E, ?], Id, E] {
      val monadErrorF: MonadError[Either[E, ?], E] = cats.instances.either.catsStdInstancesForEither
      val applicativeG: Applicative[Id] = cats.catsInstancesForId

      def controlError[A](fa: Either[E, A])(f: E => A): A = fa match {
        case Left(e) => f(e)
        case Right(a) => a
      }

      def accept[A](ga: A): Either[E, A] = Right(ga)
    }

  implicit def catsEndeavorForEitherT[F[_]: Monad, E]: ErrorControl[EitherT[F, E, ?], F, E] =
    new ErrorControl[EitherT[F, E, ?], F, E] {
      val monadErrorF: MonadError[EitherT[F, E, ?], E] = EitherT.catsDataMonadErrorForEitherT
      val applicativeG: Applicative[F] = Applicative[F]

      def controlError[A](fa: EitherT[F, E, A])(f: E => F[A]): F[A] =
        fa.value.flatMap {
          case Left(e) => f(e)
          case Right(a) => applicativeG.pure(a)
        }

      def accept[A](ga: F[A]): EitherT[F, E, A] =
        EitherT.liftF(ga)

    }

  implicit def catsEndeavorForOption: ErrorControl[Option, Id, Unit] =
    new ErrorControl[Option, Id, Unit] {
      val monadErrorF: MonadError[Option, Unit] = cats.instances.option.catsStdInstancesForOption
      val applicativeG: Applicative[Id] = cats.catsInstancesForId

      def controlError[A](fa: Option[A])(f: Unit => A): A = fa match {
        case Some(a) => a
        case None => f(())
      }

      def accept[A](ga: A): Option[A] = Some(ga)
    }

  implicit def catsEndeavorForOptionT[F[_]: Monad, E]: ErrorControl[OptionT[F, ?], F, Unit] =
    new ErrorControl[OptionT[F, ?], F, Unit] {
      val monadErrorF: MonadError[OptionT[F, ?], Unit] = new OptionTMonadError[F] {def F: Monad[F] = Monad[F]}
      val applicativeG: Applicative[F] = Applicative[F]

      def controlError[A](fa: OptionT[F, A])(f: Unit => F[A]): F[A] =
        fa.value.flatMap {
          case Some(a) => applicativeG.pure(a)
          case None => f(())
        }

      def accept[A](ga: F[A]): OptionT[F, A] =
        OptionT.liftF(ga)

    }

  implicit def catsEndeavorForStateT[F[_], G[_]: Monad, S, E]
    (implicit M: ErrorControl[F, G, E]): ErrorControl[StateT[F, S, ?], StateT[G, S, ?], E] =
    new ErrorControl[StateT[F, S, ?], StateT[G, S, ?], E] {
      implicit val F: MonadError[F, E] = M.monadErrorF
      implicit val G: Applicative[G] = M.applicativeG

      val monadErrorF: MonadError[StateT[F, S, ?], E] = IndexedStateT.catsDataMonadErrorForIndexedStateT
      val applicativeG: Applicative[StateT[G, S, ?]] = IndexedStateT.catsDataMonadForIndexedStateT

      def accept[A](ga: StateT[G, S, A]): StateT[F, S, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = M.accept(ga)
      })

      def controlError[A](fa: StateT[F, S, A])(f: E => StateT[G, S, A]): StateT[G, S, A] =
        IndexedStateT(s => M.controlError(fa.run(s))(e => f(e).run(s)))

    }

  implicit def catsEndeavorForKleisli[F[_], G[_], R, E]
  (implicit M: ErrorControl[F, G, E]): ErrorControl[Kleisli[F, R, ?], Kleisli[G, R, ?], E] =
    new ErrorControl[Kleisli[F, R, ?], Kleisli[G, R, ?], E] {
      implicit val F: MonadError[F, E] = M.monadErrorF
      implicit val G: Applicative[G] = M.applicativeG

      val monadErrorF: MonadError[Kleisli[F, R, ?], E] = Kleisli.catsDataMonadErrorForKleisli
      val applicativeG: Applicative[Kleisli[G, R, ?]] = Kleisli.catsDataApplicativeForKleisli

      def accept[A](ga: Kleisli[G, R, A]): Kleisli[F, R, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = M.accept(ga)
      })

      def controlError[A](fa: Kleisli[F, R, A])(f: E => Kleisli[G, R, A]): Kleisli[G, R, A] =
        Kleisli(r => M.controlError(fa.run(r))(e => f(e).run(r)))

    }

  implicit def catsEndeavorForWriterT[F[_], G[_], L: Monoid, E]
  (implicit M: ErrorControl[F, G, E]): ErrorControl[WriterT[F, L, ?], WriterT[G, L, ?], E] =
    new ErrorControl[WriterT[F, L, ?], WriterT[G, L, ?], E] {
      implicit val F: MonadError[F, E] = M.monadErrorF
      implicit val G: Applicative[G] = M.applicativeG

      val monadErrorF: MonadError[WriterT[F, L, ?], E] = WriterT.catsDataMonadErrorForWriterT
      val applicativeG: Applicative[WriterT[G, L, ?]] = WriterT.catsDataApplicativeForWriterT

      def accept[A](ga: WriterT[G, L, A]): WriterT[F, L, A] = ga.mapK(new (G ~> F) {
        def apply[T](ga: G[T]): F[T] = M.accept(ga)
      })

      def controlError[A](fa: WriterT[F, L, A])(f: E => WriterT[G, L, A]): WriterT[G, L, A] =
        WriterT(M.controlError(fa.run)(e => f(e).run))

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
