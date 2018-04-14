package cats.effect.unexceptional
package tests

import cats.tests.CatsSuite
import cats.Id
//import cats.effect.unexceptional.tests.TestingInstances._


class MonadBlunderSuite extends CatsSuite {

  val monadBlunderEitherLaws = new MonadBlunder.MonadBlunderLaws[Either[String, ?], Id, String, Int] {}

  test("MonadBlunder[Either].deriveAttempt") {
    forAll { (either: Either[String, Int]) =>
      monadBlunderEitherLaws.deriveAttempt(either)
    }
  }

  test("MonadBlunder[Either].deriveEnsureOr") {
    forAll { (i: Int, f: Int => String, g: Int => Boolean) =>
      monadBlunderEitherLaws.deriveEnsureOr(i)(f)(g)
    }
  }

  test("MonadBlunder[Either].deriveHandleError") {
    forAll { (either: Either[String, Int], f: String => Int) =>
      monadBlunderEitherLaws.deriveHandleError(either)(f)
    }
  }

  test("MonadBlunder[Either].bindAlwaysWorksInG") {
    forAll { (i: Int, ga: Int) =>
      monadBlunderEitherLaws.bindAlwaysWorksInG(ga, i)
    }
  }

  test("MonadBlunder[Either].raiseErrorHandleBlunderWith") {
    forAll { (s: String, f: String => Int) =>
      monadBlunderEitherLaws.raiseErrorHandleBlunderWith(s, f)
    }
  }

  test("MonadBlunder[Either].raiseErrorHandleBlunder") {
    forAll { (s: String, f: String => Int) =>
      monadBlunderEitherLaws.raiseErrorHandleBlunder(s, f)
    }
  }

  test("MonadBlunder[Either].endeavorAbsolve") {
    forAll { (either: Either[String, Int]) =>
      monadBlunderEitherLaws.endeavorAbsolve(either)
    }
  }

  test("MonadBlunder[Either].raiseErrorEndeavor") {
    forAll { (s: String) =>
      monadBlunderEitherLaws.raiseErrorEndeavor(s)
    }
  }


}
