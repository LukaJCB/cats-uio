package cats.effect.unexceptional.tests

import cats.Eq
import cats.effect.IO
import cats.effect.laws.util.TestContext
import cats.tests.CatsSuite
import cats.laws.discipline.{MonadTests, ParallelTests}
import org.scalacheck.{Arbitrary, Cogen}
import cats.effect.laws.util.TestInstances
import cats.effect.laws.discipline.arbitrary.{catsEffectLawsArbitraryForIO, catsEffectLawsArbitraryForIOParallel}
import cats.kernel.laws.discipline.MonoidTests
import cats.effect.unexceptional.Unexceptional

class UnexceptionalTestSuite extends CatsSuite {

  implicit val testContext: TestContext = TestContext()

  implicit def eqUIO[A: Eq]: Eq[Unexceptional[IO, A]] =
    TestInstances.eqIO[A].imap(Unexceptional.create)(uio => Unexceptional.runUnexceptional(uio))

  implicit def eqParUIO[A: Eq]: Eq[Unexceptional[IO.Par, A]] =
    TestInstances.eqIOPar[A]
      .imap(Unexceptional.create)(uio => Unexceptional.runUnexceptional(uio))

  implicit def arbitraryUnexceptional[A: Arbitrary: Cogen]: Arbitrary[Unexceptional[IO, A]] =
    Arbitrary(catsEffectLawsArbitraryForIO[A].arbitrary.map(io => Unexceptional.create(io)))

  implicit def arbitraryUnexceptionalPar[A: Arbitrary: Cogen]: Arbitrary[Unexceptional[IO.Par, A]] =
    Arbitrary(catsEffectLawsArbitraryForIOParallel[A].arbitrary.map(io => Unexceptional.create(io)))

  checkAll("Monoid[Unexceptional[IO, A]]", MonoidTests[Unexceptional[IO, String]].monoid)

  checkAll("Monad[Unexceptional[IO, ?]]", MonadTests[Unexceptional[IO, ?]].monad[Int, String, Int])
  //checkAll("Applicative[UIO.Par]", ApplicativeTests[UIO.Par].applicative[String, Int, Int])

  checkAll("Parallel[Unexceptional[IO, ?], Unexceptional[IO.Par, ?]]",
    ParallelTests[Unexceptional[IO, ?], Unexceptional[IO.Par, ?]].parallel[Int, String])
}
