package ru.dokwork.fasti

import cats.implicits._
import org.scalatest.FreeSpec
import org.scalatest.Matchers.{ a => _, _ }
import org.scalatest.TryValues._
import shapeless._

import scala.util.Try

class SagaSpec extends FreeSpec {

  "Saga" - {
    "should invoke `action` function and return result" in new Fixture {
      // given:
      val saga = Saga(action[A, B])
      // when:
      val result = saga(a)
      // then:
      result.success.value shouldBe Right(b)
      completedStages should contain only b
    }
    "should sequentially invoke actions" in new Fixture {
      // given:
      val saga = Saga(action[A, B]) andThen Saga(action[B, C])
      // when:
      val result = saga(a)
      // then:
      result.success.value shouldBe Right(c)
      completedStages should contain allOf(b, c)
    }

    "when continue" - {
      "should continue from the product" in new Fixture {
        // given:
        val saga: Saga[Try, A, C, B :: HNil] = Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga.continue(b :: HNil)
        // then:
        result.success.value shouldBe Right(c)
        completedStages should contain only c // because we omit first step
      }
    }

    "when compensate" - {
      "should return `Left` with a reason" in new Fixture {
        // given:
        val saga = Saga(fail[A, B])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Left(testException)
      }
      "should compensate first step for product" in new Fixture {
        // give:
        val saga = Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga.compensate(b :: HNil, testException)
        // then:
        result.success.value shouldBe Left(testException)
        completedStages shouldBe empty
        compensatedStages should contain(b)
      }
      "should return `CompensationFailed` in the context of `F`" in new Fixture {
        // given:
        val saga = Saga(action[A, B], failCompensation[B]) andThen Saga(action[B, C])
        // when:
        val result = saga.compensate(b :: HNil, testException)
        // then:
        result.failure.exception shouldBe an[CompensationFailed]
      }
      "should invoke both compensation actions" in new Fixture {
        // given:
        val saga =
          Saga(action[A, B], compensate[B]) andThen Saga(action[B, C], compensate[C]) andThen Saga(fail[C, Any])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Left(testException)
        completedStages shouldBe empty
        compensatedStages should contain theSameElementsInOrderAs List(c, b)
      }
      "should do nothing when saga failed on the beginning" in new Fixture {
        // given:
        val saga = Saga(fail[A, B]) andThen Saga(action[B, C], compensate[C])
        // when:
        val result = saga(a)
        // then:
        result.success.value shouldBe Left(testException)
        completedStages shouldBe empty
        compensatedStages shouldBe empty
      }
    }

    "with break on" - {
      "should not invoke compensation on the testException at the second step" in new Fixture {
        // given:
        val saga = Saga(action[A, B], compensate) andThen Saga(fail[B, C]).breakOn {
          case `testException` =>
        }
        // when:
        val result = saga(a)
        // then:
        result shouldBe 'failure
        compensatedStages shouldBe empty
      }
      "should invoke compensation on the testException at the second step" in new Fixture {
        // given:
        val saga = Saga(action[A, B], compensate).breakOn {
          case `testException` =>
        } andThen Saga(fail[B, C])
        // when:
        val result = saga(a)
        // then:
        result shouldBe 'success
        compensatedStages should contain only b
      }
      "should not invoke compensation on the testException at any step" in new Fixture {
        // given:
        val saga1 = (Saga(action[A, B], compensate) andThen Saga(action[B, C], compensate) andThen Saga(fail[C, D])).breakOn {
          case `testException` =>
        }
        val saga2 = (Saga(action[A, B], compensate) andThen Saga(fail[B, C]) andThen Saga(action[C, D], compensate)).breakOn {
          case `testException` =>
        }
        // when:
        val result1 = saga1(a)
        val result2 = saga2(a)
        // then:
        result1 shouldBe result2
        result1 shouldBe 'failure
        compensatedStages shouldBe empty
      }
    }
  }
}