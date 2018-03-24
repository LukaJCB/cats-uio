package cats.effect.unexceptional
package tests

import cats.Eq
import cats.effect.laws.util.TestContext
import cats.tests.CatsSuite
import cats.laws.discipline.{ApplicativeTests, MonadTests, ParallelTests}
import org.scalacheck.{Arbitrary, Cogen}
import cats.effect.laws.util.TestInstances
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import cats.kernel.laws.discipline.MonoidTests

class UIOTestSuite extends CatsSuite {

  implicit val testContext: TestContext = TestContext()

  implicit def eqUIO[A: Eq]: Eq[UIO[A]] =
    TestInstances.eqIO[A].imap(UIO.create)(uio => UIO.runUIO(uio))

  implicit def eqParUIO[A: Eq]: Eq[UIO.Par[A]] =
    eqUIO[A].imap(UIO.Par.fromUIO)(UIO.Par.toUIO)

  implicit def arbitraryUIO[A: Arbitrary: Cogen]: Arbitrary[UIO[A]] =
    Arbitrary(catsEffectLawsArbitraryForIO[A].arbitrary.map(io => UIO.create(io)))

  implicit def arbitraryParUIO[A: Arbitrary: Cogen]: Arbitrary[UIO.Par[A]] =
    Arbitrary(arbitraryUIO[A].arbitrary.map(uio => UIO.Par.fromUIO(uio)))

  checkAll("Monoid[UIO]", MonoidTests[UIO[String]].monoid)

  checkAll("Monad[UIO]", MonadTests[UIO].monad[Int, String, Int])
  checkAll("Applicative[UIO.Par]", ApplicativeTests[UIO.Par].applicative[String, Int, Int])

  checkAll("Parallel[UIO, UIO.Par]", ParallelTests[UIO, UIO.Par].parallel[Int, String])
}
