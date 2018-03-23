package cats.effect.uio
package tests

import cats.Eq
import cats.effect.laws.util.TestContext
import cats.tests.CatsSuite
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Cogen}
import cats.effect.laws.util.TestInstances
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO

class CatsuioTestSuite extends CatsSuite {

  implicit val testContext: TestContext = TestContext()

  implicit def eqUIO[A: Eq]: Eq[UIO[A]] =
    TestInstances.eqIO[A].imap(UIO.create)(uio => UIO.unwrap(uio))

  implicit def arbitraryUIO[A: Arbitrary: Cogen]: Arbitrary[UIO[A]] =
    Arbitrary(catsEffectLawsArbitraryForIO[A].arbitrary.map(io => UIO.create(io)))

  checkAll("Monad[UIO]", MonadTests[UIO].monad[Int, String, Int])
}
