package cats.effect.unexceptional

import cats.arrow.FunctionK
import cats.{Applicative, Monad, MonadError, Monoid, Parallel, Semigroup, ~>}
import cats.effect._
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration


private[unexceptional] trait NewtypeK { self =>
  private[unexceptional] type Base
  private[unexceptional] trait Tag extends Any
  type Type[F[_], A] <: Base with Tag
}

private[unexceptional] sealed abstract class UnexceptionalInstances {
  implicit def catsEffectMonadForUnexceptional[F[_]: Monad]: Monad[Unexceptional[F, ?]] =
    new Monad[Unexceptional[F, ?]] {
      def tailRecM[A, B](a: A)(f: A => Unexceptional[F, Either[A, B]]): Unexceptional[F, B] =
        Unexceptional.create(
          Monad[F].tailRecM(a)(f andThen Unexceptional.runUnexceptional))

      def flatMap[A, B](fa: Unexceptional[F, A])(f: A => Unexceptional[F, B]): Unexceptional[F, B] =
        Unexceptional.create(
          Monad[F].flatMap(Unexceptional.runUnexceptional(fa))(f andThen Unexceptional.runUnexceptional))

      def pure[A](x: A): Unexceptional[F, A] =
        Unexceptional.pure(x)
    }

  def catsEffectApplicativeForUnexceptional[G[_]: Applicative]: Applicative[Unexceptional[G, ?]] =
    new Applicative[Unexceptional[G, ?]] {
      def pure[A](x: A): Unexceptional[G, A] = Unexceptional.pure(x)

      def ap[A, B](ff: Unexceptional[G, A => B])(fa: Unexceptional[G, A]): Unexceptional[G, B] =
        Unexceptional.create(
          Applicative[G].ap(Unexceptional.runUnexceptional(ff))(Unexceptional.runUnexceptional(fa)))
    }

  implicit def catsEffectParallelForUnexceptional[F[_], G[_]](implicit P: Parallel[F, G]): Parallel[Unexceptional[F, ?], Unexceptional[G, ?]] =
      new Parallel[Unexceptional[F, ?], Unexceptional[G, ?]] {
        def applicative: Applicative[Unexceptional[G, ?]] = catsEffectApplicativeForUnexceptional(P.applicative)

        def monad: Monad[Unexceptional[F, ?]] = catsEffectMonadForUnexceptional(P.monad)

        def sequential: ~>[Unexceptional[G, ?], Unexceptional[F, ?]] =
          new ~>[Unexceptional[G, ?], Unexceptional[F, ?]] {
            def apply[A](fa: Unexceptional[G, A]): Unexceptional[F, A] =
              Unexceptional.mapK(fa)(P.sequential)
          }

        def parallel: ~>[Unexceptional[F, ?], Unexceptional[G, ?]] =
          new ~>[Unexceptional[F, ?], Unexceptional[G, ?]] {
            def apply[A](fa: Unexceptional[F, A]): Unexceptional[G, A] =
              Unexceptional.mapK(fa)(P.parallel)
          }
    }

  implicit def catsEffectMonoidForUnexceptional[F[_]: Monad, A: Monoid]: Monoid[Unexceptional[F, A]] =
    new Monoid[Unexceptional[F, A]] {
      def empty: Unexceptional[F, A] = Unexceptional.pure[F, A](Monoid[A].empty)
      def combine(x: Unexceptional[F, A], y: Unexceptional[F, A]): Unexceptional[F, A] =
        Unexceptional.create(
          Unexceptional.runUnexceptional(x).flatMap(a1 =>
            Unexceptional.runUnexceptional(y).map(a2 => Semigroup[A].combine(a1, a2))))
    }
}

object Unexceptional extends UnexceptionalInstances with NewtypeK {

  private[cats] def create[F[_], A](s: F[A]): Type[F, A] =
    s.asInstanceOf[Type[F, A]]

  def fromF[F[_]: Sync, A](fa: F[A]): Unexceptional[F, Either[Throwable, A]] =
    create(fa.attempt)

  def runUnexceptional[F[_], A](ufa: Unexceptional[F, A]): F[A] =
    ufa.asInstanceOf[F[A]]

  def runEitherIO[F[_]: Sync, A](ufa: Unexceptional[F, Either[Throwable, A]]): F[A] =
    MonadError[F, Throwable].rethrow(runUnexceptional(ufa))

  def unsafeFromF[F[_], A](fa: F[A]): Unexceptional[F, A] = create(fa)

  def pure[F[_]: Applicative, A](x: A): Unexceptional[F, A] =
    create(Applicative[F].pure(x))

  def apply[F[_]: Sync, A](x: => A): Unexceptional[F, Either[Throwable, A]] =
    fromF(Sync[F].delay(x))

  def async[F[_]: Async, A](k: (Either[Throwable, A] => Unit) => Unit): Unexceptional[F, Either[Throwable, A]] =
    fromF(Async[F].async(k))

  def suspend[F[_]: Sync, A](thunk: => Unexceptional[F, A]): Unexceptional[F, A] =
    unsafeFromF(Sync[F].suspend(runUnexceptional(thunk)))

  def unit[F[_]: Applicative]: Unexceptional[F, Unit] =
    pure(())

  def race[F[_]: Concurrent, A, B](lh: Unexceptional[F, A], rh: Unexceptional[F, B]): Unexceptional[F, Either[A, B]] =
    unsafeFromF(Concurrent[F].race(runUnexceptional(lh), runUnexceptional(rh)))


  def cancelable[F[_]: Concurrent, A](k: (Either[Throwable, A] => Unit) => IO[Unit]): Unexceptional[F, Either[Throwable, A]] =
    fromF(Concurrent[F].cancelable(k))


  def start[F[_]: Concurrent, A](ufa: Unexceptional[F, A]): Unexceptional[F, Fiber[Unexceptional[F, ?], A]] =
    unsafeFromF(Concurrent[F].start(runUnexceptional(ufa)).map(f => unFiber(f)))


  def shift[F[_]](implicit timer: Timer[Unexceptional[F, ?]]): Unexceptional[F, Unit] =
    timer.shift

  def sleep[F[_]](duration: FiniteDuration)(implicit timer: Timer[Unexceptional[F, ?]]): Unexceptional[F, Unit] =
    timer.sleep(duration)

  def mapK[F[_], G[_], A](ufa: Unexceptional[F, A])(fk: FunctionK[F, G]): Unexceptional[G, A] =
    create(fk(runUnexceptional(ufa)))

  def racePair[F[_]: Concurrent, A, B](lh: Unexceptional[F, A], rh: Unexceptional[F, B]): Unexceptional[F, Either[(A, Fiber[Unexceptional[F, ?], B]), (Fiber[Unexceptional[F, ?], A], B)]] = {
    import cats.syntax.bifunctor._
    import cats.instances.either._

    unsafeFromF(Concurrent[F].racePair(runUnexceptional(lh), runUnexceptional(rh)).map { e =>
      e.bimap({
        case (a, fiber) => (a, unFiber(fiber))
      }, {
        case (fiber, b) => (unFiber(fiber), b)
      })
    })
  }

  private def unFiber[F[_], A](f: Fiber[F, A]): Fiber[Unexceptional[F, ?], A] =
    Fiber(unsafeFromF(f.join), unsafeFromF(f.cancel))
}
