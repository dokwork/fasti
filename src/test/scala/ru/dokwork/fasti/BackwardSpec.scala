package ru.dokwork.fasti

import cats.implicits._
import org.scalatest.freespec.AnyFreeSpec
import shapeless._

import scala.util._
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

class BackwardSpec extends AnyFreeSpec with Matchers {

  import Backward._

  "Backward" - {
    "should successfully return result" in new Fixture with TryValues {
      // given:
      val backward: Backward[Try] =
        Backward(compensate[A]) compose Backward(compensate[B]) compose Backward(compensate[C]) compose Backward(
          compensate[D]
        )

      // when:
      val result = backward(c :: b :: a :: HNil, testException)

      // then:
      result shouldBe an[Success[Unit]]
    }
    "should invoke functions in the correct order" in new Fixture {
      // given:
      val backward: Backward[Try] =
        Backward(compensate[A]) compose Backward(compensate[B]) compose Backward(compensate[C]) compose Backward(
          compensate[D]
        )

      // when:
      backward(c :: b :: a :: HNil, testException)

      // then:
      compensatedStages should contain theSameElementsInOrderAs List(c, b, a)
    }
    "should return exception in the context of F on fail" in new Fixture {
      // given:
      val backward: Backward[Try] =
        Backward(failCompensation[A]) compose Backward(compensate[B]) compose Backward(compensate[C])

      // when:
      val result = backward(c :: b :: a :: HNil, testException)

      // then:
      compensatedStages should contain theSameElementsInOrderAs List(c, b)
      result shouldBe Failure(exceptionOnCompensation)
    }
  }
}
