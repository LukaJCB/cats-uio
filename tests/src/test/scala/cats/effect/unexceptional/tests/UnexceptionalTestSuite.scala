package cats.effect.unexceptional.tests

import cats.effect.IO
import cats.tests.CatsSuite
import cats.laws.discipline.{MonadTests, ParallelTests}
import cats.kernel.laws.discipline.MonoidTests
import cats.effect.unexceptional.Unexceptional
import cats.effect.unexceptional.tests.TestingInstances._
import cats.effect.laws.util.TestInstances.eqIO

class UnexceptionalTestSuite extends CatsSuite {

  checkAll("Monoid[Unexceptional[IO, A]]", MonoidTests[Unexceptional[IO, String]].monoid)

  checkAll("Monad[Unexceptional[IO, ?]]", MonadTests[Unexceptional[IO, ?]].monad[Int, String, Int])

  checkAll("Parallel[Unexceptional[IO, ?], Unexceptional[IO.Par, ?]]",
    ParallelTests[Unexceptional[IO, ?], Unexceptional[IO.Par, ?]].parallel[Int, String])
}
