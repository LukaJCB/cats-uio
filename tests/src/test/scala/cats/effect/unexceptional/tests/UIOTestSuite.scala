package cats.effect.unexceptional
package tests

import cats.tests.CatsSuite
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{MonadTests, ApplicativeTests, ParallelTests}
import cats.effect.unexceptional.tests.TestingInstances._

class UIOTestSuite extends CatsSuite {

  checkAll("Monoid[UIO]", MonoidTests[UIO[String]].monoid)

  checkAll("Monad[UIO]", MonadTests[UIO].monad[Int, String, Int])
  checkAll("Applicative[UIO.Par]", ApplicativeTests[UIO.Par].applicative[String, Int, Int])

  checkAll("Parallel[UIO, UIO.Par]", ParallelTests[UIO, UIO.Par].parallel[Int, String])
}
