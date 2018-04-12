package cats.effect.unexceptional
package tests

import cats.Eq
import cats.effect.IO
import cats.effect.laws.discipline.arbitrary.{catsEffectLawsArbitraryForIO, catsEffectLawsArbitraryForIOParallel}
import cats.effect.laws.util.TestContext
import cats.effect.laws.util.TestInstances
import cats.effect.unexceptional.{UIOImpl, Unexceptional}
import org.scalacheck.{Arbitrary, Cogen}
import scala.concurrent.{ExecutionException, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import cats.syntax.all._
import cats.instances.eq._

object TestingInstances {
  implicit val testContext: TestContext = TestContext()

  implicit def eqUIO[A: Eq]: Eq[UIO[A]] =
    TestInstances.eqIO[A].contramap(uio => UIOImpl.runUIO(uio))

  implicit def eqParUIO[A: Eq]: Eq[UIOImpl.Par[A]] =
    eqUIO[A].contramap(UIOImpl.Par.toUIO)

  implicit def arbitraryUIO[A: Arbitrary: Cogen]: Arbitrary[UIO[A]] =
    Arbitrary(catsEffectLawsArbitraryForIO[A].arbitrary.map(io => UIOImpl.create(io)))

  implicit def arbitraryParUIO[A: Arbitrary: Cogen]: Arbitrary[UIOImpl.Par[A]] =
    Arbitrary(arbitraryUIO[A].arbitrary.map(uio => UIOImpl.Par.fromUIO(uio)))


  implicit def eqUnexceptional[F[_], A: Eq](implicit E: Eq[F[A]]): Eq[Unexceptional[F, A]] =
    E.contramap(uio => Unexceptional.runUnexceptional(uio))

  implicit def eqParUnexceptionalIO[A: Eq]: Eq[Unexceptional[IO.Par, A]] =
    TestInstances.eqIOPar[A]
      .contramap(uio => Unexceptional.runUnexceptional(uio))

  implicit def equalityFuture[A](implicit A: Eq[A]): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean = {
        // Executes the whole pending queue of runnables
        testContext.tick(1.day)

        x.value match {
          case None =>
            y.value.isEmpty
          case Some(Success(a)) =>
            y.value match {
              case Some(Success(b)) => A.eqv(a, b)
              case _ => false
            }
          case Some(Failure(_)) =>
            y.value match {
              case Some(Failure(_)) =>
                // Exceptions aren't values, it's too hard to reason about
                // throwable equality and all exceptions are essentially
                // yielding non-terminating futures and tasks from a type
                // theory point of view, so we simply consider them all equal
                true
              case _ =>
                false
            }
        }
      }
    }

  implicit lazy val equalityThrowable = new Eq[Throwable] {
    override def eqv(x: Throwable, y: Throwable): Boolean = {
      val ex1 = extractEx(x)
      val ex2 = extractEx(y)
      ex1.getClass == ex2.getClass && ex1.getMessage == ex2.getMessage
    }

    // Unwraps exceptions that got caught by Future's implementation
    // and that got wrapped in ExecutionException (`Future(throw ex)`)
    def extractEx(ex: Throwable): Throwable =
      ex match {
        case ref: ExecutionException =>
          Option(ref.getCause).getOrElse(ref)
        case _ =>
          ex
      }
  }

  implicit def arbitraryUnexceptionalIO[A: Arbitrary: Cogen]: Arbitrary[Unexceptional[IO, A]] =
    Arbitrary(catsEffectLawsArbitraryForIO[A].arbitrary.map(io => Unexceptional.create(io)))

  implicit def arbitraryUnexceptionalIOPar[A: Arbitrary: Cogen]: Arbitrary[Unexceptional[IO.Par, A]] =
    Arbitrary(catsEffectLawsArbitraryForIOParallel[A].arbitrary.map(io => Unexceptional.create(io)))

}
