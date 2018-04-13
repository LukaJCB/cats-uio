package cats.effect.unexceptional
package tests

import cats.tests.CatsSuite
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{MonadTests, ApplicativeTests, ParallelTests}
import cats.effect.IO
import cats.effect.laws.util.TestInstances._
import cats.effect.laws.discipline.arbitrary._
import cats.effect.unexceptional.tests.TestingInstances._

class UIOTestSuite extends CatsSuite {

  checkAll("Monoid[UIO]", MonoidTests[UIO[String]].monoid)

  checkAll("Monad[UIO]", MonadTests[UIO].monad[Int, String, Int])
  checkAll("Applicative[UIO.Par]", ApplicativeTests[UIO.Par].applicative[String, Int, Int])

  checkAll("Parallel[UIO, UIO.Par]", ParallelTests[UIO, UIO.Par].parallel[Int, String])

  val monadBlunderIOLaws = new MonadBlunder.MonadBlunderLaws[IO, UIO, Throwable, Int] {}

  test("MonadBlunder[IO].deriveAttempt") {
    forAll { (io: IO[Int]) =>
      monadBlunderIOLaws.deriveAttempt(io)
    }
  }

  test("MonadBlunder[IO].deriveEnsureOr") {
    forAll { (uio: UIO[Int], f: Int => Throwable, g: Int => Boolean) =>
      monadBlunderIOLaws.deriveEnsureOr(uio)(f)(g)
    }
  }

  test("MonadBlunder[IO].deriveHandleError") {
    forAll { (io: IO[Int], f: Throwable => Int) =>
      monadBlunderIOLaws.deriveHandleError(io)(f)
    }
  }

  test("MonadBlunder[IO].raiseErrorHandleBlunderWith") {
    forAll { (s: Throwable, f: Throwable => UIO[Int]) =>
      monadBlunderIOLaws.raiseErrorHandleBlunderWith(s, f)
    }
  }

  test("MonadBlunder[IO].raiseErrorHandleBlunder") {
    forAll { (s: Throwable, f: Throwable => Int) =>
      monadBlunderIOLaws.raiseErrorHandleBlunder(s, f)
    }
  }

  test("MonadBlunder[IO].endeavorAbsolve") {
    forAll { (io: IO[Int]) =>
      monadBlunderIOLaws.endeavorAbsolve(io)
    }
  }

  test("MonadBlunder[IO].raiseErrorEndeavor") {
    forAll { (s: Throwable) =>
      monadBlunderIOLaws.raiseErrorEndeavor(s)
    }
  }
}
